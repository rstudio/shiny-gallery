## code to prepare `hotshot_data` dataset goes here

hotshot_data <- list(
    drivers = list(
        alexa = list(
            driver_name = "alexa",
            driver_img_url = "https://i.imgur.com/nGh4oUH.png",
            country = "us",
            cars = list(
                list(
                    car_name = "stallion",
                    car_img_url = "https://i.imgur.com/fPPYtQ0.png",
                    type = "balanced",
                    speed = 7,
                    acceleration = 7,
                    drift = 7
                ),
                list(
                    car_name = "thunder",
                    car_img_url = "https://i.imgur.com/vETmvkN.png",
                    type = "acceleration",
                    speed = 5,
                    acceleration = 9,
                    drift = 8
                ),
                list(
                    car_name = "mirage",
                    car_img_url = "https://i.imgur.com/3Etcc4C.png",
                    type = "speed",
                    speed = 9,
                    acceleration = 6,
                    drift = 7
                ),
                list(
                    car_name = "diamond_back",
                    car_img_url = "https://i.imgur.com/0TUg2zZ.png",
                    type = "drift",
                    speed = 5,
                    acceleration = 8,
                    drift = 9
                )
            )
        ),
        aston = list(
            driver_name = "aston",
            driver_img_url = "https://i.imgur.com/5KyknYM.png",
            country = "gb",
            cars = list(
                list(
                    car_name = "bulldog",
                    car_img_url = "https://i.imgur.com/liS0suC.png",
                    type = "balanced",
                    speed = 8,
                    acceleration = 8,
                    drift = 6
                ),
                list(
                    car_name = "bandit",
                    car_img_url = "https://i.imgur.com/fFGgAu5.png",
                    type = "acceleration",
                    speed = 7,
                    acceleration = 9,
                    drift = 5
                ),
                list(
                    car_name = "avenger",
                    car_img_url = "https://i.imgur.com/zE5q70i.png",
                    type = "speed",
                    speed = 10,
                    acceleration = 5,
                    drift = 7
                ),
                list(
                    car_name = "shadow",
                    car_img_url = "https://i.imgur.com/9ZsVjhN.png",
                    type = "drift",
                    speed = 6,
                    acceleration = 7,
                    drift = 9
                )
            )
        ),
        xing = list(
            driver_name = "xing",
            driver_img_url = "https://i.imgur.com/48868f2.png",
            country = "cn",
            cars = list(
                list(
                    car_name = "fastback",
                    car_img_url = "https://i.imgur.com/BAtEJJA.png",
                    type = "balanced",
                    speed = 7,
                    acceleration = 8,
                    drift = 6
                ),
                list(
                    car_name = "blaze",
                    car_img_url = "https://i.imgur.com/RmJg3U6.png",
                    type = "acceleration",
                    speed = 6,
                    acceleration = 9,
                    drift = 6
                ),
                list(
                    car_name = "alpha",
                    car_img_url = "https://i.imgur.com/C1qitYu.png",
                    type = "speed",
                    speed = 10,
                    acceleration = 7,
                    drift = 5
                ),
                list(
                    car_name = "sentinel",
                    car_img_url = "https://i.imgur.com/kMNcFEw.png",
                    type = "drift",
                    speed = 6,
                    acceleration = 6,
                    drift = 10
                )
            )
        ),
        keiko = list(
            driver_name = "keiko",
            driver_img_url = "https://i.imgur.com/2EzeVsi.png",
            country = "jp",
            cars = list(
                list(
                    car_name = "light_speed",
                    car_img_url = "https://i.imgur.com/Rahw1jX.png",
                    type = "balanced",
                    speed = 8,
                    acceleration = 8,
                    drift = 6
                ),
                list(
                    car_name = "star_fire",
                    car_img_url = "https://i.imgur.com/VEjRdEQ.png",
                    type = "acceleration",
                    speed = 6,
                    acceleration = 10,
                    drift = 6
                ),
                list(
                    car_name = "super_f-90",
                    car_img_url = "https://i.imgur.com/QDD5MiD.png",
                    type = "speed",
                    speed = 10,
                    acceleration = 8,
                    drift = 4
                ),
                list(
                    car_name = "eight_rock",
                    car_img_url = "https://i.imgur.com/OmA1CMV.png",
                    type = "drift",
                    speed = 6,
                    acceleration = 6,
                    drift = 10
                )
            )
        ),
        marcus = list(
            driver_name = "marcus",
            driver_img_url = "https://i.imgur.com/KvJMvWS.png",
            country = "jm",
            cars = list(
                list(
                    car_name = "vector",
                    car_img_url = "https://i.imgur.com/EhaaKPL.png",
                    type = "balanced",
                    speed = 6,
                    acceleration = 8,
                    drift = 7
                ),
                list(
                    car_name = "rosso",
                    car_img_url = "https://i.imgur.com/uVBsP7V.png",
                    type = "acceleration",
                    speed = 7,
                    acceleration = 9,
                    drift = 5
                ),
                list(
                    car_name = "carbon",
                    car_img_url = "https://i.imgur.com/SUneNr9.png",
                    type = "speed",
                    speed = 10,
                    acceleration = 8,
                    drift = 4
                ),
                list(
                    car_name = "mongoose",
                    car_img_url = "https://i.imgur.com/wHMSiwC.png",
                    type = "drift",
                    speed = 8,
                    acceleration = 5,
                    drift = 9
                )
            )
        ),
        viktor = list(
            driver_name = "viktor",
            driver_img_url = "https://i.imgur.com/XKK7GMi.png",
            country = "ru",
            cars = list(
                list(
                    car_name = "blade",
                    car_img_url = "https://i.imgur.com/DROEzT5.png",
                    type = "balanced",
                    speed = 7,
                    acceleration = 7,
                    drift = 7
                ),
                list(
                    car_name = "rennen",
                    car_img_url = "https://i.imgur.com/q0ohlXW.png",
                    type = "acceleration",
                    speed = 5,
                    acceleration = 9,
                    drift = 8
                ),
                list(
                    car_name = "t-66_super",
                    car_img_url = "https://i.imgur.com/NY8CQxf.png",
                    type = "speed",
                    speed = 9,
                    acceleration = 6,
                    drift = 6
                ),
                list(
                    car_name = "el_toro",
                    car_img_url = "https://i.imgur.com/YrW81Gp.png",
                    type = "drift",
                    speed = 7,
                    acceleration = 6,
                    drift = 9
                )
            )
        ),
        mike = list(
            driver_name = "mike",
            driver_img_url = "https://i.imgur.com/HPc2dSU.png",
            country = "us",
            cars = list(
                list(
                    car_name = "athena",
                    car_img_url = "https://i.imgur.com/bMMjEHB.png",
                    type = "balanced",
                    speed = 8,
                    acceleration = 6,
                    drift = 7
                ),
                list(
                    car_name = "eagle",
                    car_img_url = "https://i.imgur.com/JyMnd3a.png",
                    type = "acceleration",
                    speed = 7,
                    acceleration = 10,
                    drift = 5
                ),
                list(
                    car_name = "patriot",
                    car_img_url = "https://i.imgur.com/cI9vf04.png",
                    type = "speed",
                    speed = 9,
                    acceleration = 5,
                    drift = 7
                ),
                list(
                    car_name = "bullet",
                    car_img_url = "https://i.imgur.com/UhS9vFa.png",
                    type = "drift",
                    speed = 7,
                    acceleration = 5,
                    drift = 10
                )
            )
        ),
        toshiro = list(
            driver_name = "toshiro",
            driver_img_url = "https://i.imgur.com/ZkZy1gu.png",
            country = "jp",
            cars = list(
                list(
                    car_name = "furious",
                    car_img_url = "https://i.imgur.com/QDkBoJn.png",
                    type = "balanced",
                    speed = 8,
                    acceleration = 6,
                    drift = 7
                ),
                list(
                    car_name = "wild_line",
                    car_img_url = "https://i.imgur.com/vsAGMEW.png",
                    type = "acceleration",
                    speed = 6,
                    acceleration = 10,
                    drift = 6
                ),
                list(
                    car_name = "r-400_sport",
                    car_img_url = "https://i.imgur.com/gIFlqeQ.png",
                    type = "speed",
                    speed = 9,
                    acceleration = 7,
                    drift = 6
                ),
                list(
                    car_name = "sky_road",
                    car_img_url = "https://i.imgur.com/hW5W1XZ.png",
                    type = "drift",
                    speed = 6,
                    acceleration = 6,
                    drift = 10
                )
            )
        )
    ),
    tracks = list(
        coast = c("marina", "ocean_world", "heated_highway", "sea_view", "surf_city"),
        desert = c("area_41", "boneyard", "casino_run", "downtown", "8_ball_highway"),
        jungle = c("temple_drive", "royal_roadway", "dino_dash", "temple_ruins", "cargo_chaos"),
        mountain = c("castle_funfair", "fossil_cave", "alpine_town", "ski_paradise", "frozen_freeway")
    ),
    grand_prix = list(
        tour = c("marina", "area_41", "temple_drive", "castle_funfair"),
        pro_circuit = c("boneyard", "fossil_cave", "ocean_world", "royal_roadway"),
        racing_elite = c("dino_dash", "heated_highway", "alpine_town", "casino_run"),
        hotshot = c("ski_paradise", "temple_ruins", "downtown", "sea_view"),
        boss_level = c("8_ball_highway", "surf_city", "cargo_chaos", "frozen_freeway")
    )
)

usethis::use_data(hotshot_data, overwrite = TRUE)
