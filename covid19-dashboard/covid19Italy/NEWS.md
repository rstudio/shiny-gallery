## Version 0.3.0
* Updates for the **italy_total** dataset:

  - Added `positive_clinical_activity` - positive cases emerged from clinical activity
  - Added `positive_surveys_tests` - positive cases emerging from surveys and tests, planned at national or regional level

* Updates for the **italy_region** dataset:

  - Added `positive_clinical_activity` - positive cases emerged from clinical activity
  - Added `positive_surveys_tests` - positive cases emerging from surveys and tests, planned at national or regional level

* Updated the data refresh automation - set [docker image](https://hub.docker.com/r/rkrispin/covid19italy) to support the cron job on [Github Actions](https://github.com/RamiKrispin/covid19Italy/blob/master/.github/workflows/data_refresh_docker.yml)


## Version 0.2.0

* Automated the data refresh on the Github version with the use of Github Actions
* Add vignette - Visualization covid19italy with Choropleth Maps (non CRAN, available [here](https://covid19r.github.io/covid19italy/articles/geospatial_visualization.html))
* Update for the **italy_total** dataset:
  - `total_currently_positive` -> `cumulative_positive_cases`
  - `new_currently_positive` -> `daily_positive_cases`
  - `total_positive_cases` -> `cumulative_cases`
  - Add new columns:
      - `total_people_tested` - total number of people tested 
  
* Update for the **italy_region** dataset:
  - `total_currently_positive` -> `cumulative_positive_cases`
  - `new_currently_positive` -> `daily_positive_cases`
   - Add new columns:
      - `total_people_tested` - total number of people tested 
      - `region_spatial` - the spatial region name as in the output of the `ne_states` function from the **rnaturalearth** package

* Update for the **italy_province** dataset:
  - `total_positive_cases` was modified to `total_cases`
  -  `total_tests` was removed
  - Add new columns:
      - `new_cases` - add daily number of positive cases
      - `province_spatial` - the spatial province names as in the output of the `ne_states` function from the **rnaturalearth** package

## Version 0.1.0

* Release three datasets:
  - `italy_total` - national level
  - `italy_region` - region level
  - `italy_province` - province level
* `update_data` function to update the dataset to the most recent one


* Added a `NEWS.md` file to track changes to the package.
