#' The 2019 Novel Coronavirus COVID-19 (2019-nCoV) Italy Outbreak Dataset
#'
#' @description  daily summary of the Coronavirus (COVID-19) pandemic cases in Italy
#'
#'
#' @format A data.frame object
#' @source Presidenza del Consiglio dei Ministri - Dipartimento della Protezione Civile \href{http://www.protezionecivile.it/}{website}
#' @keywords datasets coronavirus COVID19 Italy
#' @details The dataset contains the daily summary of the Coronavirus pandemic cases in Italy. The data includes the following fields:
#'
#' - date - date of notification (Date class)
#'
#' - hospitalized_with_symptoms - number of hospitalised patients with symptoms
#'
#' - intensive_care - intensive Care
#'
#' - total_hospitalized - total hospitalised patients
#'
#' - home_confinement - home confinement
#'
#' - cumulative_positive_cases - total amount of current positive cases (Hospitalised patients + Home confinement)
#'
#' - daily_positive_cases - new amount of current positive cases (Hospitalised patients + Home confinement)
#'
#' - recovered - recovered cases
#'
#' - death - death cases
#'
#' - positive_clinical_activity - positive cases emerged from clinical activity
#'
#' - positive_surveys_tests - positive cases emerging from surveys and tests, planned at national or regional level
#'
#' - cumulative_cases - total positive cases, recovered, and death cases
#'
#' - total_tests - number of tests performed
#'
#' - total_people_tested - total number of people tested
#'
#' @examples
#'
#' data(italy_total)
#'
#' head(italy_total)
#'

"italy_total"


#' The 2019 Novel Coronavirus COVID-19 (2019-nCoV) Italy Regions Outbreak Dataset
#'
#' @description  daily summary of the Coronavirus (COVID-19) pandemic cases in Italy's regions
#'
#'
#' @format A data.frame object
#' @source Presidenza del Consiglio dei Ministri - Dipartimento della Protezione Civile \href{http://www.protezionecivile.it/}{website}
#' @keywords datasets coronavirus COVID19 Italy
#' @details The dataset contains the daily summary of the Coronavirus pandemic cases in Italy's regions. The data includes the following fields:
#'
#' - date - date of notification (Date class)
#'
#' - region_code - region code
#'
#' - region_name - region names
#'
#' - lat - latitude point
#'
#' - long - Longitude point
#'
#' - hospitalized_with_symptoms - number of hospitalised patients with symptoms
#'
#' - intensive_care - intensive Care
#'
#' - total_hospitalized - total hospitalised patients
#'
#' - home_confinement - home confinement
#'
#' - cumulative_positive_cases - total amount of current positive cases (Hospitalised patients + Home confinement)
#'
#' - daily_positive_cases - news amount of current positive cases (Hospitalised patients + Home confinement)
#'
#' - recovered - recovered cases
#'
#' - death - death cases
#'
#' - positive_clinical_activity - positive cases emerged from clinical activity
#'
#' - positive_surveys_tests - positive cases emerging from surveys and tests, planned at national or regional level
#'
#' - cumulative_cases - total positive, recovered, and death cases
#'
#' - total_tests - number of tests performed
#'
#' - total_people_tested - total number of people tested
#'
#' - region_spatial - the spatial region names as in the output of the `ne_states` function from the **rnaturalearth** package
#' @examples
#'
#' data(italy_region)
#'
#' head(italy_region)
#'

"italy_region"


#' The 2019 Novel Coronavirus COVID-19 (2019-nCoV) Italy Provinces Outbreak Dataset
#'
#' @description  daily summary of the Coronavirus (COVID-19) pandemic cases in Italy's provinces
#'
#'
#' @format A data.frame object
#' @source Presidenza del Consiglio dei Ministri - Dipartimento della Protezione Civile \href{http://www.protezionecivile.it/}{website}
#' @keywords datasets coronavirus COVID19 Italy
#' @details The dataset contains the daily summary of the Coronavirus pandemic cases in Italy's regions. The data includes the following fields:
#'
#' - date - date of notification (Date class)
#'
#' - region_code - region code
#'
#' - region_name - region names
#'
#' - province_code - province code
#'
#' - province_name - province names
#'
#' - province_abb - the province abbreviation
#'
#' - lat - latitude point
#'
#' - long - Longitude point
#'
#' - total_cases - total number of positive cases (cumulative)
#'
#' - new_cases - daily number of positive cases
#'
#' - province_spatial - the spatial province names as in the output of the `ne_states` function from the **rnaturalearth** package
#'
#' @examples
#'
#' data(italy_province)
#'
#' head(italy_province)
#'

"italy_province"



