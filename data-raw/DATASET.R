## code to prepare `DATASET` dataset goes here

demo_data <- readr::read_csv("../augmented-gaussian/data/demo_data.csv")

usethis::use_data(demo_data, overwrite = TRUE)

## code to prepare a testing output

set.seed(234)
output <- transform_data(demo_data) |> aug_fit_model(iter = 100, chain = 3)

usethis::use_data(output, internal = TRUE)
