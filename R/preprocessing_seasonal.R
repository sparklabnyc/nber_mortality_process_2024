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
output_dir <- "output_seasonal"

# Load Function
source(here("R/utils_seasonal.R"))

#1959
race_labels_1 <- c("White", "Black", "American Indian (includes Aleuts and Eskimos)", "Chinese",
                   "Japanese", "American Indian (includes Aleuts and Eskimos)", "American Indian (includes Aleuts and Eskimos)",
                   "Filipino", "Hawaiian (includes Part Hawaiian)", "Hawaiian (includes Part Hawaiian)","All other races")
race_levels_1 <- 1:11

data_1959 <- read.csv("data/mort1959.csv", fileEncoding="latin1")
summary_1959 <- data_1959 %>%
  mutate(age_group = case_when(
    nchar(age) == 3 & substr(age, 1, 1) %in% c("2", "3", "4", "5", "6") ~ "<1",
    nchar(age) == 3 & substr(age, 1, 1) == "1" ~ "85+",
    nchar(age) <= 2 ~ as.character(cut(as.numeric(age), breaks = age_bins, labels = age_labels, right = FALSE)),
    TRUE ~ "999"
  )) %>%
  select(monthdth, sex, age_group, race, stateoc, countyoc, ucod) %>%
  rename(cause = ucod, age = age_group, month = monthdth, fips = countyoc, state = stateoc) %>%
  left_join(state_codes, by = c("state" = "Code")) %>%
  mutate(state = ifelse(is.na(Abbrev), "Overseas areas", Abbrev)) %>%
  select(-Abbrev) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  group_by(month, state, fips, age, sex, race, cause) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  mutate(year = 1959) %>%
  mutate(race = factor(race, levels = race_levels_1, labels = race_labels_1)) %>%
  arrange(desc(deaths)) %>%
  mutate(fips = as.character(fips),
         fips = paste0(substr(fips, 1, nchar(fips) - 2), "0", substr(fips, nchar(fips) - 1, nchar(fips))),
         fips = as.numeric(fips))

summary_1959 <- summary_1959 %>%
  select(year, month, state, fips, age, sex, race, cause, deaths)

output_rds_path <- file.path("output_seasonal","summary_1959.rds")
saveRDS(summary_1959, file = output_rds_path)

f0_c(1960, race_levels_1, race_labels_1)
f0_c(1961, race_levels_1, race_labels_1)

#1962-1963
race_labels_2 <- c("White", "Black", "American Indian (includes Aleuts and Eskimos)", "Chinese",
                   "Japanese", "American Indian (includes Aleuts and Eskimos)", "American Indian (includes Aleuts and Eskimos)",
                   "Filipino", "Hawaiian (includes Part Hawaiian)","All other races", "Race not stated (New Jersey residents only)")
race_levels_2 <- 1:11

f_c(1962, race_levels_2, race_labels_2)
f_c(1963, race_levels_2, race_labels_2)


#1964-1967
race_labels_3 <- c("White", "Black", "American Indian (includes Aleuts and Eskimos)", "Chinese",
                   "Japanese", "Hawaiian (includes Part Hawaiian)","All other races")
race_levels_3 <- 1:7

f_c(1964, race_levels_3, race_labels_3)
f_c(1965, race_levels_3, race_labels_3)
f_c(1966, race_levels_3, race_labels_3)
f_c(1967, race_levels_3, race_labels_3)

#1968
race_labels_4 <- c("Guamanian", "White", "Black", "American Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)",
                   "All other races", "Filipino")
race_levels_4 <- 0:8

f_c(1968, race_levels_4, race_labels_4)

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
  select(monthdth, sex, age_group, race, stateoc, countyoc, ucod) %>%
  rename(cause = ucod, age = age_group, month = monthdth, fips = countyoc, state = stateoc) %>%
  mutate(state = if_else(nchar(state) == 2 & substr(state, 1, 1) == "0", substr(state, 2, 2), state)) %>%
  left_join(state_codes_1, by = c("state" = "Code")) %>%
  mutate(state = ifelse(is.na(Abbrev), "Overseas areas", Abbrev)) %>%
  select(-Abbrev) %>%
  mutate(sex = ifelse(sex == 1, "Male", "Female")) %>%
  group_by(month, state, fips, age, sex, race, cause) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  mutate(year = 1969) %>%
  mutate(race = factor(race, levels = race_levels_4, labels = race_labels_4)) %>%
  arrange(desc(deaths))

summary_1969 <- summary_1969 %>%
  select(year, month, state, fips, age, sex, race, cause, deaths)

output_rds_path <- file.path("output_seasonal", paste0("summary_1969.rds"))
saveRDS(summary_1969, file = output_rds_path)


#1970-1977
f_c(1970, race_levels_4, race_labels_4)
f_c(1971, race_levels_4, race_labels_4)
f_c(1972, race_levels_4, race_labels_4)
f_c(1973, race_levels_4, race_labels_4)
f_c(1974, race_levels_4, race_labels_4)
f_c(1975, race_levels_4, race_labels_4)
f_c(1976, race_levels_4, race_labels_4)
f_c(1977, race_levels_4, race_labels_4)

#1978-1981
race_labels_5 <- c("Other Asian or Pacific Islander", "White", "Black", "American Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)",
                   "All other races", "Filipino")
race_levels_5 <- 0:8

f_c(1978, race_levels_5, race_labels_5)
f_c(1979, race_levels_5, race_labels_5)
f_c(1980, race_levels_5, race_labels_5)
f_c(1981, race_levels_5, race_labels_5)

#1982-1988
race_labels_6 <- c("Other Asian or Pacific Islander", "White", "Black", "American Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)",
                   "All other races", "Filipino")
race_levels_6 <- 0:8

f_c(1982, race_levels_6, race_labels_6)
f_c(1983, race_levels_6, race_labels_6)
f_c(1984, race_levels_6, race_labels_6)
f_c(1985, race_levels_6, race_labels_6)
f_c(1986, race_levels_6, race_labels_6)
f_c(1987, race_levels_6, race_labels_6)
f_c(1988, race_levels_6, race_labels_6)

#1989-1991
race_labels_7 <- c("White", "Black", "American Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)", "Filipino",
                   "Other Asian or Pacific Islander", "All other races")
race_levels_7 <- 1:9

f_c(1989, race_levels_7, race_labels_7)
f_c(1990, race_levels_7, race_labels_7)
f_c(1991, race_levels_7, race_labels_7)

#1992-2002
race_labels_8 <- c("White", "Black", "American Indian (includes Aleuts and Eskimos)",
                   "Chinese", "Japanese", "Hawaiian (includes Part Hawaiian)", "Filipino",
                   "Asian Indian", "Korean", "Samoan", "Vietnamese", "Guamanian",
                   "Other Asian or Pacific Islander in areas reporting codes 18-58",
                   "Combined other Asian or Pacific Islander, includes codes 18-68 for areas that do not report them separately")
race_levels_8 <- c("1", "2", "3", "4", "5", "6", "7", "18", "28", "38", "48", "58", "68", "78")

f_c(1992, race_levels_8, race_labels_8)
f_c(1993, race_levels_8, race_labels_8)
f_c(1994, race_levels_8, race_labels_8)
f_c(1995, race_levels_8, race_labels_8)
f_c(1996, race_levels_8, race_labels_8)
f_c(1997, race_levels_8, race_labels_8)
f_c(1998, race_levels_8, race_labels_8)

#1999-2002
f_c(1999, race_levels_8, race_labels_8)
f_c(2000, race_levels_8, race_labels_8)
f_c(2001, race_levels_8, race_labels_8)
f_c(2002, race_levels_8, race_labels_8)

#2003-2004
f1_c(2003, race_levels_8, race_labels_8)
f1_c(2004, race_levels_8, race_labels_8)



files_path <- "output_seasonal"
output_path <- "output_seasonal"

rds_files <- list.files(path = files_path, pattern = "*.rds", full.names = TRUE)

combined_data <- data.frame()

for (file in rds_files) {
  data <- readRDS(file)

  data <- data %>%
    mutate(fips = as.character(fips),
           cause = as.character(cause))

  combined_data <- append(combined_data, list(data))
}

combined_data <- bind_rows(combined_data)

saveRDS(combined_data, file = file.path(output_path, "Summary_data_with_cause.rds"))


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

data_1959_1967 <- process_data_for_years_7(Summary_data_with_cause, 1959, 1967, icd_7)
data_1968_1978 <- process_data_for_years_8(Summary_data_with_cause, 1968, 1978, icd_8)
data_1979_1998 <- process_data_for_years_9(Summary_data_with_cause, 1979, 1998, icd_9)
data_1999_2004 <- process_data_for_years_10(Summary_data_with_cause, 1999, 2004, icd_10)
