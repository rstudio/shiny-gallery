## code to prepare `player_data` dataset goes here

player_data <- tibble::tribble(
  ~player_name, ~full_name, ~country, ~avatar, ~plot_emoji,
  "Wimpy", "Martin Wimpress", "gb", "Wimpy.jpg", "automobile",
  "FrenchguyCH", "Yannick Mauray", "ch", "FrenchguyCH.jpg", "sport_utility_vehicle",
  "rpodcast", "Eric Nantz", "us", "rpodcast.jpg", "racing_car",
  "bigcalm", "", "gb", "bigcalm.png", "taxi",
  "TwoD", "Henrik", "se", "TwoD.png", "articulated_lorry",
  "madhens", "Monica Ayhens-Madon", "us", "madhens.jpg", "tractor",
  "TheUnwiseGeek", "John Madon", "us", "TheUnwiseGeek.jpeg", "police_car",
  "popey", "Alan Pope", "gb", "popey.jpg", "bus",
  "bigpod", "BigPod", "si", "bigpod.png", "minibus",
  "crendo", "Unknown", "us", "crendo.png", "motor_scooter"
)


usethis::use_data(player_data, overwrite = TRUE)
