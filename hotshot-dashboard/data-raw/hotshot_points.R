## code to prepare `hotshot_points` dataset goes here

hotshot_points <- tibble::tribble(
  ~position, ~points,
  1, 12,
  2, 11,
  3, 10,
  4, 9,
  5, 8,
  6, 7,
  7, 6,
  8, 5,
  9, 4,
  10, 3,
  11, 2,
  12, 1
)

usethis::use_data(hotshot_points, overwrite = TRUE)
