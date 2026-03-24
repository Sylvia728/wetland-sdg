from elasticsearch import Elasticsearch, helpers
import csv

iso_mapping = {}
with open("/Users/silvia/geoname_index/iso_codes.csv", "r") as f:
    reader = csv.DictReader(f)
    for row in reader:
        iso_mapping[row['alpha-2']] = row['alpha-3']

es = Elasticsearch("http://localhost:9200")
BULK_SIZE = 5000

with open("/Users/silvia/geoname_index/allCountries.txt", "r") as f:
    reader = csv.reader(f, delimiter='\t')
    actions = []

    for row in reader:
        try:
            if len(row) < 19:
                continue

            coordinates = {
                "lat": float(row[4]),
                "lon": float(row[5])
            }
            country_code2 = row[8]
            country_code3 = iso_mapping.get(country_code2, "")

            doc = {
                "_index": "geonames",
                "_id": row[0],
                "_source": {
                    "geonameid": row[0],
                    "name": row[1],
                    "asciiname": row[2],
                    "alternativenames": row[3],
                    "coordinates": coordinates,
                    "feature_class": row[6],
                    "feature_code": row[7],
                    "country_code2": row[8],
                    "country_code3": country_code3,
                    "admin1_code": row[10],
                    "admin1_name": "",
                    "admin2_code": row[11],
                    "admin2_name": "",
                    "admin3_code": row[12],
                    "admin4_code": row[13],
                    "population": int(row[14]) if row[14] else 0,
                    "alt_name_length": len(row[3].split(',')),
                    "modification_date": row[18]
                }
            }
            actions.append(doc)

            if len(actions) >= BULK_SIZE:
                helpers.bulk(es, actions)
                actions = []

        except (IndexError, ValueError) as e:
            print(f"Rows with errors: {row} - {str(e)}")
            continue

    if actions:
        helpers.bulk(es, actions)