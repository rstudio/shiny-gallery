import plotly.graph_objects as go
import plotly.express as px
from pandas import DataFrame


def create_figure(
    data: DataFrame,
    year_range: list[int],
    country: str,
    y_from: str,
    title: str,
    labels: dict,
) -> go.FigureWidget:
    plot_data = data[data["Year"].between(year_range[0], year_range[1])]
    plot_data = plot_data[plot_data["Entity"].isin(country)]

    fig = px.line(
        data_frame=plot_data,
        x="Year",
        y=y_from,
        color="Entity",
        title=title,
        labels=labels,
        color_discrete_sequence=px.colors.colorbrewer.Blues[1:],
    )

    fig.update_traces(
        mode="markers+lines",
        hovertemplate=None,
        line=dict(width=5),
        marker=dict(size=15),
    )
    fig.update_layout(plot_bgcolor="white", hovermode="x unified")

    fig.update_xaxes(showline=False, gridcolor="#d2d2d2", gridwidth=0.5)
    fig.update_yaxes(showline=False, gridcolor="#d2d2d2", gridwidth=0.5)

    return go.FigureWidget(fig)
