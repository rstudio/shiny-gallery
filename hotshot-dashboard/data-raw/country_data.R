## code to prepare `country_data` dataset goes here

country_data <- tibble::tribble(
  ~country, ~label,
  "us", "United States",
  "gb", "United Kingdom",
  "cn", "China",
  "jp", "Japan",
  "jm", "Jamaica",
  "ru", "Russia",
  "ch", "Switzerland",
  "se", "Sweden",
  "si", "Slovenia"
)

country_data <- dplyr::mutate(country_data, url = glue::glue("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/{country}.svg"))

usethis::use_data(country_data, overwrite = TRUE)