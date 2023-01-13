# Respiratory Disease: Shiny for Python Version

This repository contains the result of a Py/Shiny (Shiny for Python) app-sprint during a hackathon. The goal of this sprint was to "translate" an R/Shiny application into a "Shiny for Python" version, as well as to explore some Py/Shiny-specific features. You can explore the original app as a [demo](https://connect.appsilon.com/respiratory_disease_app_sprint/). To see the original R/Shiny app code, please reach out to us. 

Note: this app was built in August 2022 with the early Alpha version of Shiny for Python. Some solutions implemented here may be resolved in future Shiny for Python versions. Please review the [official documenation](https://shiny.rstudio.com/py/docs/get-started.html) for current information. 

## Explore the app
The "normal" app is deployed at [Appsilon RSConnect](https://connect.appsilon.com/respiratory_disease_pyshiny/). The WASM version aka Shinylive is deployed at https://connect.appsilon.com/respiratory_disease_shinylive/. Please note, that for a ShinyLive application a large bundle has to be downloaded which can take some time (but it will be cached and used later - even without an internet connection).

To run the app locally, clone the repo, create a virtual environment, install the dependencies, and run the app and navigate to `localhost:8000` in the browser:

```shell
git clone git@github.com:Appsilon/respiratory_disease_pyshiny.git
cd ./respiratory_disease_pyshiny
python -m virtualenv venv
source ./venv/bin/activate
pip install -r requirements.txt
shiny run --port 8000 app.py
```

## Key Results
A [summary of the Py/Shiny app sprint](https://appsilon.com/pyshiny-demo/) can be found on our blog. You can also find a [summary of the original R/Shiny app sprint](https://appsilon.com/appsprints-r-shiny-app-development/).

- Most of the original structure/logic has been preserved, unless a direct translation was impossible
- Some UI changes have been introduced
  - Grid layout
  - Mobile responsiveness
  - Workaround for dataset switch
- A working Shinylive version of the app was created

## Challenges
- Lack of any UI-component libraries
- `ipyleaflet` lacks features of its R counterpart, and documentation is not helpful
- Issues with CPU throttling
- Shinylive version is very sensitive to the packages in requirements.txt. Problems with packages like `geopandas`.
