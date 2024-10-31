library(dplyr)

test <- read.delim("data-raw/iNaturalistResearchGradeObservations_europe.csv")

length(unique(test$speciesKey)) # 40065, schonmal überschaubarere !


