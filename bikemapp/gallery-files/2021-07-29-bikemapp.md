---
layout:       app-showcase
title:        "Bikemapp"
user_name:    Agustin Perez Santangelo
user_url:     https://2exp3.netlify.app/
date:         2021-07-29
tags:         cyclist, map, road, mobile, leaflet, sf, sp, shinyMobile
app_url:      https://2exp3.shinyapps.io/bikemapp-gallery/
source_url:   https://github.com/rstudio/shiny-gallery/bikemapp
rscloud_url:  https://rstudio.cloud/project/2539402
rscomm_url:   https://community.rstudio.com/u/agus
contest:      yes
contest-year: 2021
thumbnail:    bikemapp.png
---

This Shiny app is an interactive **map for cyclists**, aimed to be a general tool that provides information on the main things cyclists usually need to know when riding on Buenos Aires city (e.g. water faucets, bikeshops, cycleways).

The app combines data from the excellent [OpenStreetMap (OSM) collaborative project](https://download.bbbike.org/osm/bbbike/BuenosAires/) and from our [government databases](https://usig.buenosaires.gob.ar/) (which is less sparse and better curated, but limited to the federal district) to deliver a tool for all [metropolitan-area](https://es.wikipedia.org/wiki/Gran_Buenos_Aires#Regi%C3%B3n_Metropolitana_de_Buenos_Aires_(RMBA)) cylists.

From a more technical standpoint, the app uses spatial (lines, markers, circles and polygons) data objects (created with the [`sp`](https://cran.r-project.org/web/packages/sp/index.html) and [`sf`](https://cran.r-project.org/web/packages/sf/index.html) packages) that are fed to a [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html) map, which reacts to user interactions using the [Shiny](https://cran.r-project.org/web/packages/shiny/index.html) framework with [Framework7](https://framework7.io/) standalone capabilities -via [shinyMobile](https://cran.r-project.org/web/packages/shinyMobile/index.html)- which ensures functionality across mobile devices.

This app was recognized _as a runner up_ on the 2021 Shiny Contest.