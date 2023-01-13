from shiny import ui, module, reactive
from shinywidgets import (
    output_widget,
    render_widget,
)
from utils.helper_text import (
    about_text,
    slider_text_plot,
    dataset_information,
    missing_note,
)
from utils.plot_utils import create_figure
from data import plot_data_oecd, plot_data_world_bank

country_choices = plot_data_oecd["Entity"].unique().tolist() + ["World"]


@module.ui
def plot_ui():
    return ui.tags.div(
        ui.tags.div(
            about_text,
            ui.tags.hr(),
            slider_text_plot,
            ui.tags.br(),
            ui.input_slider(
                id="years_value",
                label="Select Year",
                min=1990,
                max=2017,
                value=[2010, 2015],
                sep="",
            ),
            ui.input_selectize(
                id="country_select",
                label="Select Countries:",
                choices=country_choices,
                selected="World",
                multiple=True,
            ),
            ui.tags.hr(),
            dataset_information,
            ui.tags.hr(),
            missing_note,
            class_="main-sidebar card-style",
        ),
        ui.tags.div(
            output_widget("dr_plot"),
            ui.tags.hr(),
            output_widget("pm_plot"),
            class_="main-main card-style",
        ),
        class_="main-layout",
    )


@module.server
def plot_server(input, output, session, is_wb_data):
    @reactive.Calc
    def data():
        if is_wb_data():
            return plot_data_world_bank
        return plot_data_oecd

    @reactive.Calc
    def fig_one():
        return create_figure(
            data=data(),
            year_range=input.years_value(),
            country=input.country_select(),
            y_from="Death.Rate",
            title="Death Rate From Respiratory Diseases",
            labels={
                "Year": "Year",
                "Death.Rate": "Deaths per 100,000",
            },
        )

    @reactive.Calc
    def fig_two():
        return create_figure(
            data=data(),
            year_range=input.years_value(),
            country=input.country_select(),
            y_from="PM2.5",
            title="PM2.5 Measure",
            labels={
                "Year": "Year",
                "PM2.5": "PM2.5",
            },
        )

    @output(suspend_when_hidden=False)
    @render_widget
    def dr_plot():
        return fig_one()

    @output(suspend_when_hidden=False)
    @render_widget
    def pm_plot():
        return fig_two()
