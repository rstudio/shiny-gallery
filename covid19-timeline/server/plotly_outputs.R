# empty plotly without modebar
output$everything_plot <- renderPlotly({
  config(plotly_empty(), displayModeBar = FALSE)
})


# empty plot with no modebar
output$daily_plot_confirmed <- renderPlotly({
  config(plotly_empty(), displayModeBar = FALSE)
})

# empty plot with no modebar
output$daily_plot_deaths <- renderPlotly({
  config(plotly_empty(), displayModeBar = FALSE)
})
