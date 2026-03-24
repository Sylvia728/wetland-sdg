import torch
import pandas as pd
import numpy as np
from transformers.models.paligemma.convert_paligemma_weights_to_hf import device
import wandb
import os
import random
import joblib
import nlpaug.augmenter.word as naw
from transformers import pipeline, AutoTokenizer, AutoModelForSequenceClassification, Trainer, TrainingArguments, EarlyStoppingCallback, AutoConfig
from sklearn.preprocessing import MultiLabelBinarizer
import tqdm
from torch.utils.data import Dataset,DataLoader
from sklearn.model_selection import train_test_split
from sklearn.metrics import precision_recall_curve,f1_score
from collections import defaultdict

device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")
print(f"Using device: {device}")

class SDGDataset(Dataset):
    def __init__(self, texts, labels, tokenizer, max_length=512):
        self.texts = texts
        self.labels = labels
        self.tokenizer = tokenizer
        self.max_length = max_length

    def __len__(self):
        return len(self.texts)

    def __getitem__(self, idx):
        text = str(self.texts[idx]) if pd.notna(self.texts[idx]) else ''
        encoding = self.tokenizer(
            text,
            max_length=self.max_length,
            padding='max_length',
            truncation=True,
            return_tensors='pt'
        )
        item = {
            'input_ids': encoding['input_ids'].flatten(),
            'attention_mask': encoding['attention_mask'].flatten(),
        }
        if self.labels is not None:
            item['labels'] = torch.tensor(self.labels[idx], dtype=torch.float)
        return item


# text augmentation
aug = naw.SynonymAug(aug_src='wordnet'
                    , aug_max=3
                    )

def text_augmentation(texts, prob=0.3):
    if random.random() < prob:
        return aug.augment(texts)
    return texts

# identifying specific thresholds for each label
def find_optimal_thresholds(y_true, y_prob):
    thresholds = np.zeros(y_prob.shape[1])
    for i in range(y_prob.shape[1]):
        precision, recall, thresh = precision_recall_curve(y_true[:, i], y_prob[:, i])
        f1 = (2 * precision * recall) / (precision + recall + 1e-8)
        optimal_idx = np.argmax(f1)
        thresholds[i] = thresh[optimal_idx]
    return thresholds

##################################################################
########## Model training ########################################
def train_model(training_file, model_save_dir):
    print("Start training...")
    os.makedirs(model_save_dir, exist_ok=True)

    # data cleansing
    training_df = pd.read_excel(training_file)
    training_df['SDGclass'] = training_df['SDGclass'].astype(str)
    training_df = training_df[~training_df['SDGclass'].isin(['0', 'nan', '', 'NaN'])]

    all_labels = []
    for label_str in training_df['SDGclass']:
        if pd.notna(label_str):
            cleaned_labels = [l.strip() for l in str(label_str).split(',') if l.strip() not in ['0', 'nan']]
            if len(cleaned_labels) > 0:
                all_labels.append(cleaned_labels)
        else:
            all_labels.append([])

    # binarizing labels
    mlb = MultiLabelBinarizer()
    mlb_labels = mlb.fit_transform(all_labels)
    num_labels = len(mlb.classes_)
    print(f"Number of valid labels: {num_labels}, Class:{mlb.classes_}")
    joblib.dump(mlb, os.path.join(model_save_dir, 'mlb.pkl'))

    id2label = {i: label for i, label in enumerate(mlb.classes_)}
    label2id = {label: i for i, label in enumerate(mlb.classes_)}

    texts = []
    for _, row in training_df.iterrows():
        title = row['Article Title'] if pd.notna(row['Article Title']) else ''
        abstract = row['Abstract'] if pd.notna(row['Abstract']) else ''
        raw_text = title + ' ' + abstract
        texts.append(text_augmentation(raw_text, prob=0.3))

    train_texts, val_texts, train_labels, val_labels = train_test_split(
        texts, mlb_labels, test_size=0.2, random_state=42
    )

    # calculating thresholds for each class
    def compute_metrics(p):
        try:
            preds = torch.sigmoid(torch.tensor(p.predictions)).numpy()
            labels = p.label_ids

            thresholds = find_optimal_thresholds(labels, preds)
            binary_preds = (preds >= thresholds).astype(int)

            if np.sum(binary_preds) == 0:
                return {'eval_macro_f1': 0.0, 'eval_micro_f1': 0.0}

            micro_f1 = f1_score(labels, binary_preds, average='micro', zero_division=0)
            macro_f1 = f1_score(labels, binary_preds, average='macro', zero_division=0)

        except Exception as e:
            print(f"Error: {e}")
            return {'eval_macro_f1': 0.0, 'eval_micro_f1': 0.0}

        return {
            'eval_macro_f1': macro_f1,
            'eval_micro_f1': micro_f1
        }

    wandb.init(project="classfier")

    model_name = '/Users/silvia/Desktop/bert/sciBERT/'
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModelForSequenceClassification.from_pretrained(
        model_name,
        num_labels=num_labels,
        id2label=id2label,
        label2id=label2id,
        problem_type="multi_label_classification",
        hidden_dropout_prob=0.3,
        attention_probs_dropout_prob=0.3
        ).to(device)

    assert model.config.num_labels == mlb_labels.shape[1], \
        f"Dimension mismatch: Model outputs {model.config.num_labels}, Labels have {mlb_labels.shape[1]} classes"

    # adjusting training parameters）
    training_args = TrainingArguments(
        output_dir='./results_sdg',
        optim="adamw_torch",
        learning_rate=3e-5,
        per_device_train_batch_size=8,
        per_device_eval_batch_size=8,
        num_train_epochs=15,
        weight_decay=0.05,
        evaluation_strategy="epoch",
        save_strategy="epoch",
        logging_strategy="epoch",
        load_best_model_at_end=True,
        metric_for_best_model="eval_macro_f1",
        greater_is_better=True,
        gradient_accumulation_steps=1,
        warmup_ratio=0.2,
        use_cpu=False if torch.backends.mps.is_available() else True,
        report_to="wandb"
    )

    train_dataset = SDGDataset(train_texts, train_labels, tokenizer)
    val_dataset = SDGDataset(val_texts, val_labels, tokenizer)

    class MultilabelTrainer(Trainer):
        def __init__(self, class_weights=None, **kwargs):
            super().__init__(**kwargs)
            self.class_weights = class_weights

        def compute_metrics(self,p):
            try:
                preds = torch.sigmoid(torch.tensor(p.predictions)).numpy()
                labels = p.label_ids

                thresholds = find_optimal_thresholds(labels, preds)
                binary_preds = (preds >= thresholds).astype(int)

                if np.sum(binary_preds) == 0:
                    return {'eval_macro_f1': 0.0, 'eval_micro_f1': 0.0}

                micro_f1 = f1_score(labels, binary_preds, average='micro', zero_division=0)
                macro_f1 = f1_score(labels, binary_preds, average='macro', zero_division=0)

            except Exception as e:
                print(f"Error: {e}")
                return {'eval_macro_f1': 0.0, 'eval_micro_f1': 0.0}

            return {
                'eval_macro_f1': macro_f1,
                'eval_micro_f1': micro_f1
            }

        def compute_loss(self, model, inputs, return_outputs=False, **kwargs):
            labels = inputs.pop("labels").to(model.device)
            outputs = model(**inputs)
            logits = outputs.logits

            # adopting focal loss
            gamma = 1.0
            pos_weight = torch.tensor([1.5] * num_labels).to(device)  # weight for positive samples

            bce_loss = torch.nn.BCEWithLogitsLoss(pos_weight=pos_weight)(logits, labels)
            pt = torch.exp(-bce_loss)
            loss = (1 - pt) ** gamma * bce_loss

            return (loss, outputs) if return_outputs else loss

    trainer = MultilabelTrainer(
        model=model,
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=val_dataset,
        compute_metrics=compute_metrics,
        callbacks=[EarlyStoppingCallback(early_stopping_patience=3)]
    )
    trainer.train()

    # calculating dynamic thresholds
    def calculate_dynamic_thresholds(model, val_dataset):
        val_loader = DataLoader(val_dataset, batch_size=16, shuffle=False)

        model.eval()
        all_probs, all_labels = [], []
        with torch.no_grad():
            for batch in val_loader:
                inputs = {
                    'input_ids': batch['input_ids'].to(device),
                    'attention_mask': batch['attention_mask'].to(device)
                }
                outputs = model(**inputs)
                probs = torch.sigmoid(outputs.logits).cpu().numpy()
                all_probs.append(probs)
                all_labels.append(batch['labels'].numpy())


        y_prob = np.concatenate(all_probs,axis=0)
        y_true = np.concatenate(all_labels,axis=0)

        model.save_pretrained(model_save_dir)
        tokenizer.save_pretrained(model_save_dir)

        thresholds = find_optimal_thresholds(y_true, y_prob)
        return model, tokenizer,np.array(thresholds, dtype=np.float32)

    print("Calculating dynamic thresholds...")
    val_dataset = SDGDataset(val_texts, val_labels, tokenizer)
    model, tokenizer, optimal_thresholds = calculate_dynamic_thresholds(model, val_dataset)

    model.save_pretrained(model_save_dir)
    tokenizer.save_pretrained(model_save_dir)

    np.save(os.path.join(model_save_dir, 'class_thresholds.npy'), optimal_thresholds)

    return model, tokenizer, optimal_thresholds


##################################################################
########## Processing classifier #################################
def predict_sdgs(df, model, tokenizer, model_save_dir):
    print("Starting predicting SDG classification...")
    thresholds_path = os.path.join(model_save_dir, 'class_thresholds.npy')
    if not os.path.exists(thresholds_path):
        raise FileNotFoundError("Class thresholds file not found!")

    mlb = joblib.load(os.path.join(model_save_dir, 'mlb.pkl'))

    texts = []
    for _, row in df.iterrows():
        title = row['Article Title'] if pd.notna(row['Article Title']) else ''
        abstract = row['Abstract'] if pd.notna(row['Abstract']) else ''
        raw_text = title + ' ' + abstract
        if len(raw_text.strip()) == 0:
            raw_text = "[EMPTY]"
        texts.append(raw_text)

    dataset = SDGDataset(texts, None, tokenizer)
    print(f"Valid texts waiting for prediction: {len(dataset)}")
    if len(dataset) == 0:
        raise ValueError("No valid text found!")
    dataloader = DataLoader(dataset, batch_size=16, shuffle=False)

    model.eval()
    all_probs = []
    device = model.device
    with torch.no_grad():
        try:
            for batch in tqdm.tqdm(dataloader, desc="Progress"):
                inputs = {
                    'input_ids': batch['input_ids'].to(device),
                    'attention_mask': batch['attention_mask'].to(device)
                }
                outputs = model(**inputs)
                probs = torch.sigmoid(outputs.logits).detach().cpu().numpy()
                all_probs.append(probs)
        except RuntimeError as e:
            if 'out of memory' in str(e):
                print("Out of memory")
            raise

    all_probs = np.concatenate(all_probs, axis=0)

    # Checking dimensions
    class_thresholds = np.load(thresholds_path)
    if all_probs.shape[1] != class_thresholds.shape[0]:
        raise ValueError(f"Mismatched! Model output dimension is {all_probs.shape[1]}, threshold dimension is {class_thresholds.shape[0]}")

    binary_preds = (all_probs >= class_thresholds).astype(int)
    df['Predicted_SDGs'] = [
        ",".join(mlb.classes_[np.where(row)[0]]) if np.any(row) else "/"
        for row in binary_preds
    ]

    label_counts = defaultdict(int)
    for labels in df['Predicted_SDGs']:
        for l in labels.split(','):
            label_counts[l] += 1

    print("Classification finished! Counts:")
    for k, v in sorted(label_counts.items(), key=lambda x: -x[1]):
        print(f"SDG {k}: {v}labels")

    return df


#%%
def main(data_files):
    device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")
    print("Starting...\n")
    model_save_dir = '/Users/silvia/Desktop/bert/sdg_classifier'
    # checking model existence
    if not os.path.exists(os.path.join(model_save_dir, 'model.safetensors')):
        print("WET-SDG classifier model not found, starting training...")
        training_file = '/.../training.xls'
        model, tokenizer, threshold = train_model(training_file, model_save_dir)
    else:
        print("Loading WET-SDG classifier model...")
        model = AutoModelForSequenceClassification.from_pretrained(model_save_dir)
        tokenizer = AutoTokenizer.from_pretrained(model_save_dir)
        threshold_path = os.path.join(model_save_dir, 'class_thresholds.npy')

    for file_path in data_files:
        print(f"\nProcessing {file_path}")
        df = pd.read_excel(file_path)
        result_df = predict_sdgs(df, model, tokenizer, model_save_dir)
        base_path = os.path.splitext(file_path)[0]
        output_path = f"{base_path}_classified.xlsx"
        result_df.to_excel(output_path, index=False)
        print(f"Result saved to: {output_path}")

if __name__ == "__main__":
    data_files = [
        '/.../SDG1_1000.xls',
        '/.../SDG1_2000.xls',
        '/.../SDG1_3000.xls',...
    ]
    main(data_files)