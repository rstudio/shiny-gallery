# Pulling raw data
update_italy_total <- function(branch = "master"){

  `%>%` <- magrittr::`%>%`
print(branch)

if(branch != "master" && branch != "dev"){
  stop("The branch argument is missing")
}
italy_total <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv",
                        stringsAsFactors = FALSE) %>%
  stats::setNames(c("date_temp", "state",
                    "hospitalized_with_symptoms","intensive_care",
                    "total_hospitalized", "home_confinement",
                    "cumulative_positive_cases", "daily_positive_cases",
                    "daily_cases", "recovered",
                    "death", "positive_clinical_activity",
                    "positive_surveys_tests",
                    "cumulative_cases",
                    "total_tests", "total_people_tested",
                    "notes_it")) %>%
  dplyr::mutate(date = lubridate::ymd(substr(date_temp, 1, 10))) %>%
  dplyr::select(-date_temp, -state, -notes_it,  -daily_cases) %>%
  dplyr::select(date, dplyr::everything()) %>%
  dplyr::arrange(date)


if(ncol(italy_total) != 14){
  stop("The number of columns is invalid")
} else if(nrow(italy_total) < 140){
  stop("The number of raws does not match the minimum number of rows")
} else if(min(italy_total$date) != as.Date("2020-02-24")){
  stop("The starting date is invalid")
}

italy_total_csv <- read.csv(sprintf("https://raw.githubusercontent.com/RamiKrispin/covid19Italy/%s/csv/italy_total.csv", branch) , stringsAsFactors = FALSE) %>%
  dplyr::mutate(date = as.Date(date))

if(ncol(italy_total_csv) != 14){
  stop("The number of columns is invalid")
} else if(nrow(italy_total_csv) < 140){
  stop("The number of raws does not match the minimum number of rows")
} else if(min(italy_total_csv$date) != as.Date("2020-02-24")){
  stop("The starting date is invalid")
}


if(nrow(italy_total) > nrow(italy_total_csv)){
  print("Updates available")
  usethis::use_data(italy_total, overwrite = TRUE)
  write.csv(italy_total, "csv/italy_total.csv", row.names = FALSE)
  print("The national dataset was updated")
} else {
  print("Updates are not available")
}

return(print("Done..."))

}
