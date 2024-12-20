# data scriping ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


library(dplyr)
library(rgbif)

# Funcktion to scrape data ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
process_species <- function(speciesKey) {
  # Load existing data
  if (file.exists("data-raw/plants_1.rds")) {
    old_data <- readRDS("data-raw/plants_1.rds")
    old_data$speciesKey <- as.character(old_data$speciesKey) # Convert speciesKey to character
  } else {
    old_data <- data.frame()
  }

  # Check if the speciesKey has already been processed
  if (!is.null(old_data) && any(grepl(speciesKey, old_data$speciesKey, fixed = TRUE))) {
    print(paste("Skipping already processed speciesKey:", speciesKey))
    return(NULL)  # Skip processing this speciesKey
  }

  # print(paste("Starting processing for:", speciesKey))

  # Search for records
  data <- occ_search(
    speciesKey = speciesKey,
    mediaType = 'StillImage',
    limit = 3,
    hasGeospatialIssue = FALSE,
    datasetName = "iNaturalist Research-grade Observations"
  )

  # Process data if available
  if (!is.null(data$data) && nrow(data$data) > 0) {
    data1 <- data$data %>%
      mutate(
        scientificName = if ("scientificName" %in% names(.)) scientificName else NA_character_,
        speciesKey = as.character(speciesKey),
        issues = if ("issues" %in% names(.)) issues else NA_character_,
        continent = if ("continent" %in% names(.)) continent else NA_character_,
        countryCode = if ("countryCode" %in% names(.)) countryCode else NA_character_,
        stateProvince = if ("stateProvince" %in% names(.)) stateProvince else NA_character_,
        eventDate = if ("eventDate" %in% names(.)) eventDate else NA_character_,
        family = if ("family" %in% names(.)) family else NA_character_,
        order = if ("order" %in% names(.)) order else NA_character_,
        occurrenceRemarks = if ("occurrenceRemarks" %in% names(.)) occurrenceRemarks else NA_character_,
        identificationRemarks = if ("identificationRemarks" %in% names(.)) identificationRemarks else NA_character_,
      ) %>%
      select(scientificName, speciesKey, species, genus, family, order, issues, continent, countryCode, stateProvince, eventDate, occurrenceRemarks, identificationRemarks)

    # Extract and match media URLs
    media_urls <- sapply(data$media, function(media_entry) {
      if (length(media_entry[[1]]) > 0 && !is.null(media_entry[[1]][[1]]$identifier)) {
        media_entry[[1]][[1]]$identifier
      } else {
        NA
      }
    })
    if (length(media_urls) < nrow(data1)) {
      media_urls <- c(media_urls, rep(NA, nrow(data1) - length(media_urls)))
    }
    data1 <- bind_cols(data1, media_url = media_urls)
  } else {
    data1 <- data.frame(scientificName = as.character(NA),
                        speciesKey = as.character(speciesKey), issues = as.character(NA),
                        continent = as.character(NA), countryCode = as.character(NA), media_url = as.character(NA))
  }

  # Filter and download images
  data1 <- data1 %>%
    filter(!is.na(media_url)) %>%
    group_by(speciesKey) %>%
    mutate(
      image_id = row_number(),
      # local_path = ifelse(!is.na(media_url),
      #                     {
      #                       file_name <- paste0("images/", gsub(" ", "_", speciesKey), "_", image_id, ".jpeg")
      #                       download.file(media_url, file_name, mode = "wb")
      #                       file_name
      #                     },
      #                     NA)
    ) %>%
    ungroup() %>%
    select(-image_id)

  # Function to retrieve German common names for a list of species keys
  get_german_names <- function(species_keys) {
    sapply(species_keys, function(key) {
      # Fetch vernacular names for the given species key
      common_names <- name_usage(key = key, data = "vernacularNames")

      # Filter for German common names
      german_name <- tryCatch({
        common_names$data %>%
          filter(language == "deu") %>%
          pull(vernacularName)
      }, error = function(e) {
        return(NULL)
      })

      # Return the first German name if available, otherwise NA
      if (length(german_name) > 0) {
        return(german_name[1])
      } else {
        return(NA)
      }
    }, USE.NAMES = FALSE)
  }

  # Fetch German names and combine with old data
  if (nrow(data1) > 0) {
    data1$species_german <- tryCatch({
      get_german_names(data1$speciesKey)
    }, error = function(e) {
      rep(NA, nrow(data1))
    })
  } else {
    data1$species_german <- as.character(NA)
  }

  print(paste("Finished processing for:", speciesKey))
  new_data <- bind_rows(old_data, data1)
  saveRDS(new_data, "data-raw/plants_1.rds")
}


# species_list from a gbif dataset ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
species_list <- read.delim2("data-raw/iNaturalistResearchGradeObs_Germany_all.csv") %>% select(speciesKey) %>% unique() %>% filter(!is.na(speciesKey)) # %>% arrange(speciesKey)


# apply function to species_list ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- ---
pbapply::pblapply(species_list$speciesKey, process_species)






# error at speciesKey 5242768, or rather te one after it!
which(species_list$speciesKey[332:2500]==5242768)
species_list$speciesKey[332:2500][395] # check
# i.e. 395 + 1 throws the error

pbapply::pblapply(species_list$speciesKey[728:2500], process_species)


# error at speciesKey 2515148, or rather te one after it!
which(species_list$speciesKey[332:2500]==2515148)
species_list$speciesKey[332:2500][1310] # check
# i.e. 395 + 1 throws the error
332+1310 + 1
# 1643

pbapply::pblapply(species_list$speciesKey[1640:4064], process_species)

# 1346 arten mit deutschem namen




# data saving ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

plants <- readRDS("data-raw/plants_1.rds")
usethis::use_data(plants, overwrite = TRUE)





# data saving specific ----
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

data <- readRDS("data-raw/plants_1.rds")
# filter for hymenoptera
hymenoptera <- data %>%
  filter(order=="Hymenoptera")

usethis::use_data(hymenoptera, overwrite = TRUE)
