setwd("/Users/yuanyulu/nber_mortality_process_2024")

output_dir <- "output"

age_labels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
age_bins <- c(-Inf, 1, seq(5, 85, by = 5), Inf)

process_and_save_data <- function(year, race_levels, race_labels) {
  file_path <- file.path("data", paste0("mort", year, ".csv"))
  data <- read.csv(file_path, fileEncoding="latin1")
  summary_data <- data %>%
    mutate(age_group = case_when(
      nchar(age) == 3 & substr(age, 1, 1) %in% c("2", "3", "4", "5", "6") ~ "<1",
      nchar(age) == 3 & substr(age, 1, 1) == "1" ~ "85+",
      nchar(age) <= 2 ~ as.character(cut(as.numeric(age), breaks = age_bins, labels = age_labels, right = FALSE)),
      TRUE ~ "999"
    )) %>%
    select(ucod, monthdth, sex, age_group, race, countyoc) %>%
    rename(age = age_group, cause = ucod, fips = countyoc) %>%
    group_by(cause, monthdth, sex, age, race, fips) %>%
    summarise(deaths = n(), .groups = 'drop') %>%
    mutate(year = year) %>%
    mutate(race = factor(race, levels = race_levels, labels = race_labels)) %>%
    arrange(desc(deaths))
  output_rds_path <- file.path(output_dir, paste0("summary_", year, ".rds"))
  saveRDS(summary_data, file = output_rds_path)
}
