## code to prepare `my_dataset` dataset goes here
my_dataset <- readRDS("data-raw/data_4.rds")
usethis::use_data(my_dataset, overwrite = TRUE)
