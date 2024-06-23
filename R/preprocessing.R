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
output_dir <- "output"

# Load Function
source(here("R/utils.R"))

#1959
race_labels_1 <- c("White", "Black", "Indian", "Chinese",
                   "Japanese", "Aleut", "Eskimo",
                   "Filipino", "Hawaiian", "Part-Hawaiian","All other races")
race_levels_1 <- 1:11

data_1959 <- read.csv("data/mort1959.csv", fileEncoding="latin1")
summary_1959 <- data_1959 %>%
  mutate(age_group = case_when(
    nchar(age) == 3 & substr(age, 1, 1) %in% c("2", "3", "4", "5", "6") ~ "<1",
    nchar(age) == 3 & substr(age, 1, 1) == "1" ~ "85+",
    nchar(age) <= 2 ~ as.character(cut(as.numeric(age), breaks = age_bins, labels = age_labels, right = FALSE)),
    TRUE ~ "999"
  )) %>%
  select(ucod, monthdth, sex, age_group, race, stateoc, countyoc) %>%
  rename(age = age_group, cause = ucod, fips = countyoc, state = stateoc) %>%
  left_join(state_codes, by = c("state" = "Code")) %>%
  mutate(race = ifelse(state %in% c("CA", "MI") & race == 6, 11, race)) %>%
  select(-state) %>%
  rename(state = Abbrev) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  group_by(cause, monthdth, sex, age, race, state, fips) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  mutate(year = 1959) %>%
  mutate(race = factor(race, levels = race_levels_1, labels = race_labels_1)) %>%
  arrange(desc(deaths))

output_rds_path <- file.path("output","summary_1959.rds")
saveRDS(summary_1959, file = output_rds_path)

#1960-1961

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

#1968
race_labels_4 <- c("Guamian", "White", "Black", "Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)",
                   "All other races", "Filipino")
race_levels_4 <- 0:8

process_and_save_data(1968, race_levels_4, race_labels_4)

#1969
data_1969 <- read_sas("data/mort1969.sas7bdat")

state_codes_1 <- state_codes %>%
  mutate(Code = as.character(Code))

summary_1969 <- data_1969 %>%
  mutate(age_group = case_when(
    nchar(age) == 3 & substr(age, 1, 1) %in% c("2", "3", "4", "5", "6") ~ "<1",
    nchar(age) == 3 & substr(age, 1, 1) == "1" ~ "85+",
    nchar(age) <= 2 ~ as.character(cut(as.numeric(age), breaks = age_bins, labels = age_labels, right = FALSE)),
    TRUE ~ "999"
  )) %>%
  select(ucod, monthdth, sex, age_group, race, stateoc, countyoc) %>%
  rename(age = age_group, cause = ucod, fips = countyoc) %>%
  mutate(state = if_else(nchar(stateoc) == 2 & substr(stateoc, 1, 1) == "0", substr(stateoc, 2, 2), stateoc)) %>%
  left_join(state_codes_1, by = c("state" = "Code")) %>%
  mutate(state = ifelse(is.na(Abbrev), "Overseas areas", Abbrev)) %>%
  select(-Abbrev) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  group_by(cause, monthdth, sex, age, race, state, fips) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  mutate(year = 1969) %>%
  mutate(race = factor(race, levels = race_levels_4, labels = race_labels_4)) %>%
  arrange(desc(deaths))

output_rds_path <- file.path("output", paste0("summary_1969.rds"))
saveRDS(summary_1969, file = output_rds_path)


#1970-1977
process_and_save_data(1970, race_levels_4, race_labels_4)
process_and_save_data(1971, race_levels_4, race_labels_4)
process_and_save_data(1972, race_levels_4, race_labels_4)
process_and_save_data(1973, race_levels_4, race_labels_4)
process_and_save_data(1974, race_levels_4, race_labels_4)
process_and_save_data(1975, race_levels_4, race_labels_4)
process_and_save_data(1976, race_levels_4, race_labels_4)
process_and_save_data(1977, race_levels_4, race_labels_4)

#1978-1981
race_labels_5 <- c("Other Asian or Pacific Islander", "White", "Black", "Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)",
                   "All other races", "Filipino")
race_levels_5 <- 0:8

process_and_save_data(1978, race_levels_5, race_labels_5)
process_and_save_data(1979, race_levels_5, race_labels_5)
process_and_save_data(1980, race_levels_5, race_labels_5)
process_and_save_data(1981, race_levels_5, race_labels_5)

#1982-1988
race_labels_6 <- c("Other Asian or Pacific Islander", "White", "Black", "American Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)",
                   "All other races", "Filipino")
race_levels_6 <- 0:8

process_and_save_data(1982, race_levels_6, race_labels_6)
process_and_save_data(1983, race_levels_6, race_labels_6)
process_and_save_data(1984, race_levels_6, race_labels_6)
process_and_save_data(1985, race_levels_6, race_labels_6)
process_and_save_data(1986, race_levels_6, race_labels_6)
process_and_save_data(1987, race_levels_6, race_labels_6)
process_and_save_data(1988, race_levels_6, race_labels_6)

#1989-1991
race_labels_7 <- c("White", "Black", "American Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)", "Filipino",
                   "Other Asian or Pacific Islander", "All other races")
race_levels_7 <- 1:9

process_and_save_data(1989, race_levels_7, race_labels_7)
process_and_save_data(1990, race_levels_7, race_labels_7)
process_and_save_data(1991, race_levels_7, race_labels_7)

#1992-2002
race_labels_8 <- c("White", "Black", "American Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)", "Filipino",
                   "Asian Indian", "Korean", "Samoan", "Vietnamese", "Guamanian",
                   "Other Asian or Pacific Islander in areas reporting codes 18-58",
                   "Combined other Asian or Pacific Islander, includes codes 18-68 for areas that do not report them separately")
race_levels_8 <- c("1", "2", "3", "4", "5", "6", "7", "18", "28", "38", "48", "58", "68", "78")

process_and_save_data(1992, race_levels_8, race_labels_8)
process_and_save_data(1993, race_levels_8, race_labels_8)
process_and_save_data(1994, race_levels_8, race_labels_8)
process_and_save_data(1995, race_levels_8, race_labels_8)
process_and_save_data(1996, race_levels_8, race_labels_8)
process_and_save_data(1997, race_levels_8, race_labels_8)
process_and_save_data(1998, race_levels_8, race_labels_8)
process_and_save_data(1999, race_levels_8, race_labels_8)
process_and_save_data(2000, race_levels_8, race_labels_8)
process_and_save_data(2001, race_levels_8, race_labels_8)
process_and_save_data(2002, race_levels_8, race_labels_8)

#2003-2004

process_and_save_data_2(2003, race_levels_8, race_labels_8)
process_and_save_data_2(2004, race_levels_8, race_labels_8)


#cause decode
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
data_1999_2004 <- process_data_for_years_10(total_deaths_per_year_cause, 1999, 2004, icd_10)
deaths_per_year_cause <- bind_rows(data_1959_1967, data_1968_1978, data_1979_1998, data_1999_2004)
summary_deaths_per_year_cause <- deaths_per_year_cause %>%
  group_by(year, Category) %>%
  summarise(total_deaths = sum(total_deaths), .groups = 'drop')
category_totals <- summary_deaths_per_year_cause %>%
  group_by(Category) %>%
  summarise(total_deaths_sum = sum(total_deaths), .groups = 'drop') %>%
  arrange(desc(total_deaths_sum))
summary_deaths_per_year_cause$Category <- factor(summary_deaths_per_year_cause$Category,
                                                 levels = category_totals$Category)
