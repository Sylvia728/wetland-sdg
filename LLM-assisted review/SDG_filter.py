import torch
import pandas as pd
import numpy as np
import wandb
import os
import random
import nlpaug.augmenter.word as naw
from transformers import pipeline, AutoTokenizer, AutoModelForSequenceClassification, Trainer, TrainingArguments, EarlyStoppingCallback
import tqdm
from torch.utils.data import Dataset
from sklearn.model_selection import train_test_split
from sklearn.metrics import precision_recall_curve,precision_score


def read_data(file_path):
    print(f"Reading: {file_path}...")
    df = pd.read_excel(file_path)
    print(f"Data rows: {len(df)}")
    return df

def remove_duplicates(df, id_column='UT (Unique ID)'):
    print("Removing duplicates...")
    initial_rows = len(df)
    df_filtered = df.drop_duplicates(subset=id_column)
    removed_rows = initial_rows - len(df_filtered)
    print(f" {removed_rows} rows removed, {len(df_filtered)} rows left")
    return df_filtered

##################################################################
########## Model training#########################################
def train_model(training_file, model_save_dir):
    print("Starting training...")

    training_df = pd.read_excel(training_file)
    training_df['label'] = training_df['SDGclass'].apply(lambda x: 0 if str(x) == '0' else 1)

    # setting text augmentation with a certain probability
    aug = naw.SynonymAug(aug_src='wordnet'
                         , aug_max=3
                         )

    def text_augmentation(text, prob):
        if random.random() < prob:
            return aug.augment(text)
        return text

    texts = []
    labels = []
    for _, row in training_df.iterrows():
        title = row['Article Title'] if pd.notna(row['Article Title']) else ''
        abstract = row['Abstract'] if pd.notna(row['Abstract']) else ''
        raw_text = title + ' ' + abstract
        # 30% probability of text augmentation
        texts.append(text_augmentation(raw_text, prob=0.3))
        labels.append(row['label'])

    # dividing training and validating dataset
    train_texts, val_texts, train_labels, val_labels = train_test_split(texts, labels, test_size=0.2, random_state=42)

    class RelevanceDataset(Dataset):
        def __init__(self, texts, labels, tokenizer, max_length):
            self.texts = texts
            self.labels = labels
            self.tokenizer = tokenizer
            self.max_length = max_length

        def __len__(self):
            return len(self.texts)

        def __getitem__(self, idx):
            encoding = self.tokenizer(
                self.texts[idx],
                max_length=self.max_length,
                padding='max_length',
                truncation=True,
                return_tensors='pt'
            )
            return {
                'input_ids': encoding['input_ids'].flatten(),
                'attention_mask': encoding['attention_mask'].flatten(),
                'labels': torch.tensor(self.labels[idx], dtype=torch.long)
            }

    wandb.init(project="SDG_filter")

    model_name = '/.../sciBERT/'
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    model = AutoModelForSequenceClassification.from_pretrained(model_name,
                                                               num_labels=2,
                                                               hidden_dropout_prob=0.2, #increasing dropout prob
                                                               attention_probs_dropout_prob=0.2
                                                               )

    train_dataset = RelevanceDataset(train_texts, train_labels, tokenizer)
    val_dataset = RelevanceDataset(val_texts, val_labels, tokenizer)

    def compute_metrics(eval_pred):
        predictions, labels = eval_pred
        predictions = np.argmax(predictions, axis=1)
        return {"precision": precision_score(labels, predictions)}

    # adjusting training parameters
    training_args = TrainingArguments(
        output_dir='./results',
        learning_rate=5e-5,
        num_train_epochs=10,
        per_device_train_batch_size=16,
        per_device_eval_batch_size=16,
        gradient_accumulation_steps=1,
        logging_dir='./logs',
        gradient_checkpointing=True,
        optim="adamw_torch_fused",  # optimizer
        max_grad_norm=1.0,  # adopting gradient clipping
        weight_decay=0.01,  # enhancing L2 regularization
        adam_beta1=0.9,
        adam_beta2=0.999,
        lr_scheduler_type="cosine",  # cosine decay
        warmup_ratio=0.2,  # warming up
        eval_strategy="epoch",
        save_strategy="epoch",
        logging_strategy="epoch",
        load_best_model_at_end=True,
        metric_for_best_model="eval_precision",
        greater_is_better=True
    )

    training_args.report_to = ["wandb"]
    def log_metrics(metrics):
        wandb.log(metrics)

    # training model
    trainer = Trainer(
        model=model,
        args=training_args,
        train_dataset=train_dataset,
        eval_dataset=val_dataset,
        compute_metrics=compute_metrics
    )
    trainer.add_callback(EarlyStoppingCallback(
        early_stopping_patience=2,  # increasing patience
        early_stopping_threshold=0.01
    ))

    trainer.train()

    # adaptively calculating thresholds
    val_predictions = trainer.predict(val_dataset)
    val_preds = val_predictions.predictions
    val_probs = torch.softmax(torch.tensor(val_preds), dim=1).numpy()[:, 1]
    val_labels = val_predictions.label_ids
    precision, recall, thresholds = precision_recall_curve(val_labels, val_probs)
    f1_scores = 2 * (precision * recall) / (precision + recall + 1e-8)
    best_threshold = thresholds[np.argmax(f1_scores)]

    # saving fine-tuned model
    model.save_pretrained(model_save_dir)
    tokenizer.save_pretrained(model_save_dir)
    with open(os.path.join(model_save_dir, 'threshold.txt'), 'w') as f:
        f.write(str(best_threshold))

    print(f"Training finished,the best threshold is: {best_threshold:.4f}")
    return model, tokenizer, best_threshold


##################################################################
########## Processing filter #####################################
def filter_relevant_papers(df,model,tokenizer,threshold):
    print("Starting filtering...")

    classifier = pipeline(
        "text-classification",
        model=model,
        tokenizer=tokenizer,
        device=0,
        truncation=True,
        max_length=512
    )

    def is_relevant(row):
        title = row['Article Title'] if pd.notna(row['Article Title']) else ''
        abstract = row['Abstract'] if pd.notna(row['Abstract']) else ''
        text = title + ' ' + abstract
        torch.cuda.empty_cache()  #cleaning memory
        result = classifier(text, top_k=None)

        scores = [item['score'] for item in result]
        labels = [item['label'] for item in result]

        if scores and labels:
            max_score = max(scores)
            max_label = labels[scores.index(max_score)]
            if max_label == 'LABEL_1' and max_score >= threshold:
                return 1
            else:
                return 0
        else:
            return 0

    # batch processing
    batch_size = 32
    df['relevant'] = 0
    for i in tqdm.tqdm(range(0, len(df), batch_size), desc="Progress"):
        batch = df.iloc[i:i + batch_size]
        df.loc[batch.index, 'relevant'] = batch.apply(is_relevant, axis=1)
        torch.cuda.empty_cache()

    relevant_papers = df[df['relevant'] == 1]
    print(f"Filtering finished! Number of relevant publications: {len(relevant_papers)}")

    return relevant_papers

#%%
def main(data_files):
    print("Starting...\n")
    model_save_dir = '/.../sdg_relevance'
    # checking model existence
    if not os.path.exists(os.path.join(model_save_dir, 'model.safetensors')):
        print("WET-SDG filter model not found, starting training...")
        training_file = '/.../training.xls'
        model, tokenizer, threshold = train_model(training_file, model_save_dir)
    else:
        print("Loading WET-SDG filter model...")
        model = AutoModelForSequenceClassification.from_pretrained(model_save_dir)
        tokenizer = AutoTokenizer.from_pretrained(model_save_dir)
        with open(os.path.join(model_save_dir, 'threshold.txt'), 'r') as f:
            threshold = float(f.read())

    for file_path in data_files:
        print(f"\nProcessing {file_path}")
        df = read_data(file_path)
        df = remove_duplicates(df)
        relevant_papers = filter_relevant_papers(df, model, tokenizer, threshold)
        output_path = os.path.join('/.../', f'filtered_{os.path.basename(file_path).replace(".xls", ".xlsx")}')
        relevant_papers.to_excel(output_path, index=False,engine='openpyxl')
        print(f"Result saved to: {output_path}")


if __name__ == "__main__":
    data_files = [
        '/.../SDG1_1000.xls',
        '/.../SDG1_2000.xls',
        '/.../SDG1_3000.xls',...
    ]
    main(data_files)
