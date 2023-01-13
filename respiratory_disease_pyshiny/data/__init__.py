from pandas import read_csv


polygon_data = read_csv("data/countries.csv")
polygon_data["coordinates"] = polygon_data["coordinates"].apply(eval)
points_from_polygons = read_csv("data/points_from_polygons.csv")

map_data_world_bank = (
    read_csv("data/map_data_world_bank.csv")
    .drop(["lng", "lat"], axis=1)
    .merge(points_from_polygons, on="Code")
)
map_data_oecd = (
    read_csv("data/map_data_oecd.csv")
    .drop(["lng", "lat"], axis=1)
    .merge(points_from_polygons, on="Code")
)

plot_data_world_bank = read_csv("data/plot_data_world_bank.csv")
plot_data_oecd = read_csv("data/plot_data_oecd.csv")
