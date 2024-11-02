library(dplyr)
library(rgbif)

process_species <- function(speciesKey) {
  # Load existing data
  if (file.exists("data_5.rds")) {
    old_data <- readRDS("data_5.rds")
    old_data$speciesKey <- as.character(old_data$speciesKey) # Convert speciesKey to character
  } else {
    old_data <- data.frame()
  }

  # Check if the speciesKey has already been processed
  if (!is.null(old_data) && any(grepl(speciesKey, old_data$speciesKey, fixed = TRUE))) {
    print(paste("Skipping already processed speciesKey:", speciesKey))
    return(NULL)  # Skip processing this speciesKey
  }

  print(paste("Starting processing for:", speciesKey))

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
        scientificName = as.character(scientificName),
        speciesKey = as.character(speciesKey),
        issues = as.character(issues),
        continent = as.character(continent),
        countryCode = as.character(countryCode),
        family = if ("family" %in% names(.)) family else NA_character_,
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
    data1 <- data.frame(scientificName = as.character(scientificName),
                        speciesKey = as.character(NA), speciesKey = NA, issues = NA,
                        continent = NA, countryCode = NA, media_url = NA)
  }

  # Filter and download images
  data1 <- data1 %>%
    filter(!is.na(media_url)) %>%
    group_by(speciesKey) %>%
    mutate(
      image_id = row_number(),
      local_path = ifelse(!is.na(media_url),
                          {
                            file_name <- paste0("images/", gsub(" ", "_", speciesKey), "_", image_id, ".jpeg")
                            download.file(media_url, file_name, mode = "wb")
                            file_name
                          },
                          NA)
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
    data1$species_german <- NA
  }

  print(paste("Finished processing for:", speciesKey))
  new_data <- bind_rows(old_data, data1)
  saveRDS(new_data, "data_5.rds")
  return(data1)
}

# Loop over species list
# species_list <- c("Boletus edulis", "Imleria badia", "Laccaria amethystina")
# lapply(species_list, process_species)

species_list <- read.delim2("data-raw/iNaturalistResearchGradeObservations_europe.csv") %>% select(speciesKey) %>% unique() %>% filter(!is.na(speciesKey)) %>% arrange(speciesKey)

# species_list_1_5 <- species_list$speciesKey[1:5]
species_list_1_100 <- species_list$speciesKey[1:100]
# species_list_101_200 <- species_list$speciesKey[101:200] # done
lapply(species_list_1_100, process_species)



