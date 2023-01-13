import geopandas
import pandas as pd

gdf = geopandas.read_file("data/countries.geojson")

# important not to use `to_dict()` methid
json = eval(gdf.set_index("id").to_json())

ids = list(map(lambda feature: feature["id"], json["features"]))
types = list(
    map(lambda feature: feature["geometry"]["type"], json["features"])
)
coordinates = list(
    map(lambda feature: feature["geometry"]["coordinates"], json["features"])
)

df = pd.DataFrame({"id": ids, "type": types, "coordinates": coordinates})
df.to_csv("data/countries.csv", index=False)
