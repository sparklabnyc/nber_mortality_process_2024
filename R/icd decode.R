# Load necessary packages
library(dplyr)
library(here)
library(haven)
library(stringr)
library(readr)
library(purrr)

# Set working directory
project_root <- here::here()
setwd(project_root)

# Load Function
source(here("R/utils.R"))

total_deaths_per_year_cause <- data.frame(
  year = integer(),
  cause = character(),
  total_deaths = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  total_deaths <- summary_data %>%
    group_by(cause) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(year = year,
           cause = as.character(cause)) %>%
    arrange(desc(total_deaths)) %>%
    slice_head(n = 50)

  total_deaths_per_year_cause <- bind_rows(total_deaths_per_year_cause, total_deaths)
}

icd_7 <- read_csv("data/icd7.csv")
icd_8 <- read_csv("data/icd8.csv")
icd_9 <- read_csv("data/icd9.csv")
icd_10 <- read_csv("data/icd10.csv")

icd_7 <- icd_7 %>%
  mutate(Code = if_else(str_starts(Code, "E"), str_remove(Code, "^E"), Code)) %>%
  mutate(Code = as.character(Code)) %>%
  mutate(Code = if_else(Code == lag(Code) & !is.na(lag(Code)), paste0(lag(Code), ".0"), Code))

icd_8 <- icd_8 %>%
  mutate(Code = if_else(Code == "000", "0", Code)) %>%
  mutate(Code = if_else(str_detect(Code, "^000\\."), str_replace(Code, "^000", "0"), Code)) %>%
  mutate(Code = if_else(nchar(Code) > 1 & !str_detect(Code, "^0\\."), str_remove(Code, "^0+"), Code)) %>%
  mutate(Code = if_else(str_starts(Code, "E"), str_remove(Code, "^E"), Code)) %>%
  mutate(Code = if_else(Code == lag(Code) & !is.na(lag(Code)), paste0(Code, ".0"), Code)) %>%
  mutate(Code = str_replace_all(Code, "\\.", ""))

icd_9 <- icd_9 %>%
  mutate(Code = if_else(str_starts(Code, "E"), str_remove(Code, "^E"), Code)) %>%
  mutate(Code = as.character(Code)) %>%
  mutate(Code = if_else(Code == lag(Code) & !is.na(lag(Code)),
                        if_else(str_detect(Code, "\\."),
                                paste0(Code, "0"),
                                paste0(Code, ".0")),
                        Code)) %>%
  mutate(Code = if_else(Code == lag(Code) & !is.na(lag(Code)),
                        if_else(str_detect(Code, "\\."),
                                paste0(Code, "0"),
                                paste0(Code, ".0")),
                        Code))

data_1959_1967 <- process_data_for_years_7(total_deaths_per_year_cause, 1959, 1967, icd_7)
data_1968_1978 <- process_data_for_years_8(total_deaths_per_year_cause, 1968, 1978, icd_8)
data_1979_1998 <- process_data_for_years_9(total_deaths_per_year_cause, 1979, 1998, icd_9)
data_1999_2022 <- process_data_for_years_10(total_deaths_per_year_cause, 1999, 2022, icd_10)
deaths_per_year_cause <- bind_rows(data_1959_1967, data_1968_1978, data_1979_1998, data_1999_2022)
summary_deaths_per_year_cause <- deaths_per_year_cause %>%
  group_by(year, Category) %>%
  summarise(total_deaths = sum(total_deaths), .groups = 'drop')
category_totals <- summary_deaths_per_year_cause %>%
  group_by(Category) %>%
  summarise(total_deaths_sum = sum(total_deaths), .groups = 'drop') %>%
  arrange(desc(total_deaths_sum))
summary_deaths_per_year_cause$Category <- factor(summary_deaths_per_year_cause$Category,
                                                 levels = category_totals$Category)
