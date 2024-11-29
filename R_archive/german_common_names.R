# library(rgbif)
# library(dplyr)
#
# test <- read.delim("data-raw/iNaturalistResearchGradeObservations_europe.csv")
#
# data <- readRDS("data-raw/data_4.rds")
# length(unique(data$speciesKey))
#
# # Function to retrieve German common names for a list of species keys
# get_german_names <- function(species_keys) {
#   sapply(species_keys, function(key) {
#     # Fetch vernacular names for the given species key
#     common_names <- name_usage(key = key, data = "vernacularNames")
#
#     german_name <- tryCatch({
#       common_names$data %>%
#       filter(language == "deu") %>%
#       pull(vernacularName)
#     }, error = function(e) {
#       return(NULL)
#     })
#
#     # Return the first German name if available, otherwise NA
#     if (length(german_name) > 0) {
#       return(german_name[1])
#     } else {
#       return(NA)
#     }
#   }, USE.NAMES = FALSE)
# }
#
# gbif::name_usage(key = data$speciesKey[1], data = "vernacularNames")
#
#
# # Replace with your speciesKey
# species_key <- data$speciesKey[10]
#
# # Step 1: Get the taxonomic hierarchy of the species
# species_info <- name_usage(key = species_key)
#
# # Extract genusKey and familyKey
# genus_key <- species_info$data$genusKey
# family_key <- species_info$data$familyKey
#
# # Step 2: Get the German name for the genus
# name_usage(key = genus_key, data = "vernacularNames")
# genus_name <- name_usage(key = genus_key, data = "vernacularNames") %>%
#   dplyr::filter(language == "de") %>%
#   dplyr::select(vernacularName)
#
#
#
#
#
# # Name usage for a taxonomic name
# name_usage(name='Puma', rank="GENUS")
#
# # Name usage for a taxonomic name
# a <- name_usage(key='9703', language ="deu")
#
#
#
# # Function to retrieve German common names for a list of species keys
# data$speciesKey[1:5]
# get_german_names(data$speciesKey[1:99])
#
#
# # Fetch German names and combine with old data
# if (nrow(data) > 0) {
#   data$species_german <- tryCatch({
#     get_german_names(data$speciesKey)
#   }, error = function(e) {
#     rep(NA, nrow(data))
#   })
# } else {
#   data$species_german <- NA
# }
#
# data1 <- data %>%
#   select(speciesKey, species_german)
#
#
#
#
#
#
#
#
#
# species_list <- read.delim2("data-raw/iNaturalistResearchGradeObservations_europe.csv") %>% select(speciesKey) %>% unique() %>% filter(!is.na(speciesKey)) %>% arrange(speciesKey)
# species_list_1_5 <- species_list$speciesKey[1:5] # done
#
# common_names <- name_usage(key = species_list$speciesKey[3], data = "vernacularNames")
#
# # Filter for German common names
# tryCatch({
#   common_names$data %>%
#     filter(language == "deu") %>%
#     pull(vernacularName)
# }, error = function(e) {
#   return(NULL)
# })

