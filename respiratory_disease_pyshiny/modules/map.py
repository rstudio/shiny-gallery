from typing import cast
from pandas import DataFrame
from shiny import ui, module, reactive
from shinywidgets import output_widget, register_widget

from ipywidgets import Layout
from ipyleaflet import Map, LayerGroup, basemaps

from utils.helper_text import (
    about_text,
    missing_note,
    dataset_information,
    slider_text_map,
)
from utils.map_utils import add_circles, add_polygons, filter_data
from data import polygon_data, map_data_oecd, map_data_world_bank

basemap = cast(dict, basemaps)


@module.ui
def map_ui():
    return ui.tags.div(
        ui.tags.div(
            about_text,
            ui.tags.hr(),
            slider_text_map,
            ui.tags.br(),
            ui.input_slider(
                id="years_value",
                label="Select Year",
                min=1990,
                max=2017,
                value=2010,
                sep="",
            ),
            ui.tags.hr(),
            dataset_information,
            ui.tags.hr(),
            missing_note,
            class_="main-sidebar card-style",
        ),
        ui.tags.div(
            output_widget("map", width="auto", height="auto"),
            class_="main-main card-style no-padding",
        ),
        class_="main-layout",
    )


@module.server
def map_server(input, output, session, is_wb_data):
    # Initialize and display when the session starts (1)
    map = Map(
        basemap=basemap["CartoDB"]["Positron"],
        center=(50, 10),
        zoom=5,
        scroll_wheel_zoom=True,
        min_zoom=3,
        max_zoom=18,
        no_wrap=True,
        layout=Layout(width="100%", height="100%"),
    )
    map.panes = {"circles": {"zIndex": 650}, "choropleth": {"zIndex": 750}}
    register_widget("map", map)

    # Circles Layer will later be filled with circleMarkers
    circle_markers_layer = LayerGroup()
    circle_markers_layer.pane = "circles"
    map.add_layer(circle_markers_layer)

    # Polygon layer will later be filled reactively
    choropleth_layer = LayerGroup()
    choropleth_layer.pane = "choropleth"
    map.add_layer(choropleth_layer)

    @reactive.Calc
    def point_data() -> DataFrame:
        if is_wb_data():
            return filter_data(map_data_world_bank, input.years_value())
        return filter_data(map_data_oecd, input.years_value())

    @reactive.Effect
    def _() -> None:
        add_circles(point_data(), circle_markers_layer)

    @reactive.Effect()
    def _() -> None:
        add_polygons(polygon_data, point_data(), choropleth_layer)
