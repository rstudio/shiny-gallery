[![eRum2020::CovidR](https://badgen.net/https/runkit.io/erum2020-covidr/badge/branches/master/hahn-covid-shinyline?cache=300)](https://milano-r.github.io/erum2020-covidr-contest/hahn-covid-shinyline.html)

# Covid Shinyline: The 2019-20 Coronavirus Pandemic visualized  with R and Shiny
Covid Shinyline uses the data from Johns Hopkins University to visualize the outbreak of the novel coronavirus. The data is available from [this repository](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data).

You can scroll through the story so far, look at the countries currently leading in various statistics and view the figures on a world map.

The app uses the [mapbox API](https://docs.mapbox.com/api/). If you have an API key, save it in a **key.env** file that looks like this:
```{bash}
MAPBOX_KEY=YOUR_SECRET_API_KEY
```
If you do not have such a key, the app will display a dark basemap from carto.
# Run it on your machine
You can run Covid Shinyline on your own machine using the following code:
```R
packages = c(
	"data.table", "dotenv", "dplyr", "english", "forecast", "fresh", "highcharter", "htmltools", "leaflet",
	"leaflet.extras", "plotly", "plyr", "quantmod", "readr", "sass", "sf", "shiny", "shinyanimate", "shinybusy",
	"shinyjs", "shinyWidgets", "stringr", "waiter"
	)
install.packages(packages, repos = "https://cran.rstudio.com/")
devtools::install_github("rstudio/leaflet.mapboxgl")
library(shiny)
runGitHub("covid_shiny", "nicoFhahn")
```
# Screenshots
![alt text](https://i.imgur.com/yP3b7eA.png "Logo Title Text 1")
![alt text](https://i.imgur.com/BLrMttV.png "Logo Title Text 1")

### Acknowledgments
The folks over at RStudio with the [SuperZIP demo](https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example) which heavily inspired the layout of the leaflet map.

### Questions/issues/contact
nico@f-hahn.de, or open a GitHub issue
