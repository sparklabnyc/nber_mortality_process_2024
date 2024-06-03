library(dplyr)
setwd("/Users/yuanyulu/nber_mortality_process_2024")
age_labels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
age_bins <- c(-Inf, 1, seq(5, 85, by = 5), Inf)
output_dir <- "output"

#1959-1961
race_labels_1 <- c("White", "Black", "Indian", "Chinese",
                   "Japanese", "Aleut", "Eskimo",
                   "Filipino", "Hawaiian", "Part-Hawaiian","All other races")
race_levels_1 <- 1:11

process_and_save_data(1959, race_levels_1, race_labels_1)
process_and_save_data(1960, race_levels_1, race_labels_1)
process_and_save_data(1961, race_levels_1, race_labels_1)

#1962-1963
race_labels_2 <- c("White", "Black", "Indian", "Chinese",
                   "Japanese", "Aleut", "Eskimo",
                   "Filipino", "Hawaiian (includes Part Hawaiian)","All other races", "Race not stated (New Jersey residents only)")
race_levels_2 <- 1:11

process_and_save_data(1962, race_levels_2, race_labels_2)
process_and_save_data(1963, race_levels_2, race_labels_2)

#1964-1967
race_labels_3 <- c("White", "Black", "Indian (includes Aleuts and Eskimos)", "Chinese",
                   "Japanese", "Hawaiian (includes Part Hawaiian)","All other races")
race_levels_3 <- 1:7

process_and_save_data(1964, race_levels_3, race_labels_3)
process_and_save_data(1965, race_levels_3, race_labels_3)
process_and_save_data(1966, race_levels_3, race_labels_3)
process_and_save_data(1967, race_levels_3, race_labels_3)

#1968-1978
race_labels_4 <- c("Guamian", "White", "Black", "Indian (includes Aleuts and Eskimos)",
                 "Chinese", "Japanese", "Hawaiian (includes part-Hawaiian)",
                 "All other races", "Filipino")
race_levels_4 <- 0:8

process_and_save_data(1968, race_levels_4, race_labels_4)
process_and_save_data(1969, race_levels_4, race_labels_4)
process_and_save_data(1970, race_levels_4, race_labels_4)
process_and_save_data(1971, race_levels_4, race_labels_4)
process_and_save_data(1972, race_levels_4, race_labels_4)
process_and_save_data(1973, race_levels_4, race_labels_4)
process_and_save_data(1974, race_levels_4, race_labels_4)
process_and_save_data(1975, race_levels_4, race_labels_4)
process_and_save_data(1976, race_levels_4, race_labels_4)
process_and_save_data(1977, race_levels_4, race_labels_4)
process_and_save_data(1978, race_levels_4, race_labels_4)

#1979

race_labels_5 <- c("Other Asian or Pacific Islander", "White", "Black", "Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes part-Hawaiian)",
                   "All other races", "Filipino")
race_levels_5 <- 0:8

process_and_save_data(1979, race_levels_5, race_labels_5)
