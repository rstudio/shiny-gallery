from ipywidgets import HTML
from branca.colormap import linear
from pandas import DataFrame, merge
from numpy import isnan
from ipyleaflet import CircleMarker, LayerGroup, Choropleth


def determine_circle_radius(num: float) -> int:
    """Circles are scaled differently in ipyleaflet compared to R/Leaflet,
    hence different coefficients.
    """
    num = int(num)

    bins = [range(10), range(10, 20), range(20, 25)]
    coefficients = [1.1, 0.8, 0.75]
    final_coef = 0.2

    for bin, coef in zip(bins, coefficients):
        if num in bin:
            return int(num * coef)
    return int(num * final_coef)


def determine_circle_color(num: float) -> str:
    if isnan(num):
        return "#D2D2D2"
    num = int(num)

    bins = [
        range(1),
        range(1, 2),
        range(2, 5),
        range(5, 10),
        range(10, 20),
        range(20, 50),
        range(50, 100),
    ]
    colors = [
        "#F7FCF0",
        "#E0F3DB",
        "#CCEBC5",
        "#A8DDB5",
        "#7BCCC4",
        "#4EB3D3",
        "#2B8CBE",
    ]
    final_color = "#08589E"

    for bin, color in zip(bins, colors):
        if num in bin:
            return color
    return final_color


def add_circles(geodata: DataFrame, circle_layer: LayerGroup) -> None:
    """Layer data is updated by reference, hence None return"""
    circle_layer.clear_layers()
    circle_markers = []
    for _, row in geodata.iterrows():
        popup = HTML(
            f"<b>{row.Entity}:</b></br>" + str(round(row["Death.Rate"], 2))
        )
        circle_marker = CircleMarker(
            location=[row["lat"], row["lng"]],
            radius=determine_circle_radius(row["Death.Rate"]),
            weight=1,
            color="white",
            opacity=0.7,
            fill_color=determine_circle_color(row["PM2.5"]),
            fill_opacity=0.5,
            popup=popup,
        )
        circle_markers.append(circle_marker)
    points = LayerGroup(layers=circle_markers)
    circle_layer.add_layer(points)


def add_polygons(
    polygon_data: DataFrame,
    points_data: DataFrame,
    polygons_layer: LayerGroup,
) -> None:
    polygons_layer.clear_layers()
    combined_data = merge(
        polygon_data, points_data, left_on="id", right_on="Code"
    )
    geo_data = dataframe_to_geojson(combined_data)
    choro_data = dict(zip(combined_data["id"], combined_data["Death.Rate"]))
    choropleth_layer = Choropleth(
        geo_data=geo_data,
        choro_data=choro_data,
        colormap=linear.GnBu_09,  # pyright: ignore
        value_min=0,
        value_max=70,
        style={
            "weight": 2,
            "opacity": 1,
            "fillOpacity": 0.5,
            "color": "white",
            "dashArray": "3",
        },
        hover_style={
            "weight": 1,
            "color": "#FFF",
            "dashArray": "",
            "fillOpacity": 0.8,
            "bringToFront": False,
        },
    )
    polygons_layer.add_layer(choropleth_layer)


def filter_data(data: DataFrame, year: int) -> DataFrame:
    return data[data["Year"] == year]


def dataframe_to_geojson(df: DataFrame) -> dict:
    geojson = {"type": "FeatureCollection", "features": []}
    for _, row in df.iterrows():
        feature = {
            "type": "Feature",
            "id": row["id"],
            "properties": {},
            "geometry": {
                "type": row["type"],
                "coordinates": row["coordinates"],
            },
        }
        geojson["features"].append(feature)
    return geojson
