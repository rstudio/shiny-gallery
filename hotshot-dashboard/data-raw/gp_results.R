## code to prepare `gp_results` dataset goes here

gp_results <- read.csv("inst/extdata/gp_results.csv", header = TRUE, stringsAsFactors = FALSE)
gp_results <- tibble::as_tibble(gp_results)

usethis::use_data(gp_results, overwrite = TRUE)
