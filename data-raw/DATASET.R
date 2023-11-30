## code to prepare `DATASET` dataset goes here

demo_data <- readr::read_csv("../augmented-gaussian/data/demo_data.csv")

usethis::use_data(demo_data, overwrite = TRUE)
