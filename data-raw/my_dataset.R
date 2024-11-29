## code to prepare `my_dataset` dataset goes here
my_dataset <- readRDS("data-raw/data_6.rds")
usethis::use_data(my_dataset, overwrite = TRUE)
