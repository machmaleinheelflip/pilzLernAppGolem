devtools::load_all(".")

library(readxl)
shroomGroups <- read_excel("data-raw/Lamellenpilze_Habitustypen_n_Merkmale.xlsx",
                                                    sheet = "Zuordnungen") %>%
  # select(genus = Gattung_sci, key1) %>%
  dplyr::rename(genus = Gattung_sci) %>%
  distinct() %>% filter(!is.na(genus))
# TODO ansscheinind sind manche gattungen in mehreren gruppen, versuche diese zu finden und versuche herauszufinden, wie sie Björn einordnet!

usethis::use_data(shroomGroups, overwrite = TRUE)

shrooms_extended <- shrooms %>%
  left_join(Lamellenpilze_Habitustypen_n_Merkmale, by = c("genus" = "Gattung_sci"), relationship = "one-to-many")


## checks
# nonMatching <- shrooms_extended %>%
#   select(genus, key1) %>% filter(is.na(key1)) %>% pull(genus) %>% unique() %>% sort() %>% as.character()
#
# matching <- shrooms_extended %>%
#   select(genus, key1) %>% filter(!is.na(key1)) %>% pull(genus) %>% unique() %>% sort() %>% as.character()
# matching
