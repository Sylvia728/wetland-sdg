import os
import pandas as pd
from mordecai import Geoparser
from tqdm import tqdm
import logging
import tensorflow as tf

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'
logging.getLogger('tensorflow').setLevel(logging.ERROR)
tf.get_logger().setLevel('ERROR')
tqdm.pandas()

def extract_geo_info(row, geo):
    title = row.get('Article Title', '') if pd.notna(row.get('Article Title', '')) else ''
    abstract = row.get('Abstract', '') if pd.notna(row.get('Abstract', '')) else ''
    text = f"{title} {abstract}".strip()
    text_lower = text.lower()

    try:
        results = geo.geoparse(text, verbose=0)
    except Exception as e:
        print(f"Error: {e} in '{text}'")
        results = []

    seen = set()
    data = {
        '_original_index': row.name,
        'word_list': [],
        'country_predicted_list': [],
        'admin1_list': [],
        'coordinates_list': [],
        'place_name_list': []
    }

    for res in results:
        word = res.get('word', '')
        if not word or word.lower() not in text_lower:
            continue

        geo_info = res.get('geo', {})
        lat = geo_info.get('lat')
        lon = geo_info.get('lon')
        if lat is None or lon is None:
            continue

        identifier = (word.lower(), res.get('country_predicted'), f"{lat},{lon}")
        if identifier in seen:
            continue
        seen.add(identifier)

        data['word_list'].append(word)
        data['country_predicted_list'].append(res.get('country_predicted'))
        data['admin1_list'].append(geo_info.get('admin1'))
        data['coordinates_list'].append(f"{lat},{lon}")
        data['place_name_list'].append(geo_info.get('place_name'))

    return pd.Series(data)


def process_file(file_path, geo, output_dir, batch_size=100):
    df = pd.read_excel(file_path).reset_index(drop=True)
    if not {'Article Title', 'Abstract'}.issubset(df.columns):
        print(f"Skip {file_path}")
        return

    chunks = [df.iloc[i:i + batch_size].copy() for i in range(0, len(df), batch_size)]
    processed_chunks = []

    for chunk in tqdm(chunks, desc=f"Processing {os.path.basename(file_path)}", unit="chunk"):
        chunk = chunk.reset_index(drop=True)
        geo_df = chunk.progress_apply(lambda r: extract_geo_info(r, geo), axis=1)
        merged = chunk.merge(geo_df, left_index=True, right_on='_original_index').drop(columns=['_original_index'])
        processed_chunks.append(merged)

    result = pd.concat(processed_chunks, ignore_index=True)
    base_name = os.path.splitext(os.path.basename(file_path))[0]
    output_path = os.path.join(output_dir, f"geo_{base_name}.xlsx")
    result.to_excel(output_path, index=False)
    print(f"Finished! Results saved in {output_path}\n")


def main(data_files, output_dir='./geo', batch_size=100):
    geo = Geoparser(es_port=9200, es_timeout=30)
    os.makedirs(output_dir, exist_ok=True)
    for file_path in data_files:
        process_file(file_path, geo, output_dir, batch_size)


if __name__ == "__main__":
    folder_path = '/.../classsifiedpapers/'
    data_files = [
        os.path.join(folder_path, f)
        for f in os.listdir(folder_path)
        if f.split("_")[-1].split(".")[0].isdigit() and 56 <= int(f.split("_")[-1].split(".")[0]) <= 66
    ]
    main(data_files, output_dir='/.../classsifiedpapers/geo', batch_size=100)