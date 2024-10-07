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
race_labels_1 <- c("White", "Black or African American", "American Indian or Alaska Native", "Asian or Pacific Islander",
                   "Asian or Pacific Islander", "American Indian or Alaska Native", "American Indian or Alaska Native",
                   "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander","All other races")
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

f(1960, race_levels_1, race_labels_1)
f(1961, race_levels_1, race_labels_1)

#1962-1963
race_labels_2 <- c("White", "Black or African American", "American Indian or Alaska Native", "Asian or Pacific Islander",
                   "Asian or Pacific Islander", "American Indian or Alaska Native", "American Indian or Alaska Native",
                   "Asian or Pacific Islander", "Asian or Pacific Islander","All other races", "Race not stated")
race_levels_2 <- 1:11

f1(1962, race_levels_2, race_labels_2)
f1(1963, race_levels_2, race_labels_2)


#1964-1967
race_labels_3 <- c("White", "Black or African American", "American Indian or Alaska Native", "Asian or Pacific Islander",
                   "Asian or Pacific Islander", "Asian or Pacific Islander","All other races")
race_levels_3 <- 1:7

f1(1964, race_levels_3, race_labels_3)
f1(1965, race_levels_3, race_labels_3)
f1(1966, race_levels_3, race_labels_3)
f1(1967, race_levels_3, race_labels_3)

#1968
race_labels_4 <- c("Asian or Pacific Islander", "White", "Black or African American", "American Indian or Alaska Native",
                   "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander",
                   "All other races", "Asian or Pacific Islander")
race_levels_4 <- 0:8

f1(1968, race_levels_4, race_labels_4)

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
f1(1970, race_levels_4, race_labels_4)
f1(1971, race_levels_4, race_labels_4)
f1(1972, race_levels_4, race_labels_4)
f1(1973, race_levels_4, race_labels_4)
f1(1974, race_levels_4, race_labels_4)
f1(1975, race_levels_4, race_labels_4)
f1(1976, race_levels_4, race_labels_4)
f1(1977, race_levels_4, race_labels_4)

#1978-1981
race_labels_5 <- c("Asian or Pacific Islander", "White", "Black or African American", "American Indian or Alaska Native",
                   "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander",
                   "All other races", "Asian or Pacific Islander")
race_levels_5 <- 0:8

f1(1978, race_levels_5, race_labels_5)
f1(1979, race_levels_5, race_labels_5)
f1(1980, race_levels_5, race_labels_5)
f1(1981, race_levels_5, race_labels_5)

#1982-1988
race_labels_6 <- c("Asian or Pacific Islander", "White", "Black or African American", "American Indian or Alaska Native",
                   "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander",
                   "All other races", "Asian or Pacific Islander")
race_levels_6 <- 0:8

f1(1982, race_levels_6, race_labels_6)
f1(1983, race_levels_6, race_labels_6)
f1(1984, race_levels_6, race_labels_6)
f1(1985, race_levels_6, race_labels_6)
f1(1986, race_levels_6, race_labels_6)
f1(1987, race_levels_6, race_labels_6)
f1(1988, race_levels_6, race_labels_6)

#1989-1991
race_labels_7 <- c("White", "Black or African American", "American Indian or Alaska Native",
                   "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander",
                   "Asian or Pacific Islander", "All other races")
race_levels_7 <- 1:9

f1(1989, race_levels_7, race_labels_7)
f1(1990, race_levels_7, race_labels_7)
f1(1991, race_levels_7, race_labels_7)

#1992-2002
race_labels_8 <- c("White", "Black or African American", "American Indian or Alaska Native",
                   "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander",
                   "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander", "Asian or Pacific Islander",
                   "Asian or Pacific Islander",
                   "Asian or Pacific Islander")
race_levels_8 <- c("1", "2", "3", "4", "5", "6", "7", "18", "28", "38", "48", "58", "68", "78")

f1(1992, race_levels_8, race_labels_8)
f1(1993, race_levels_8, race_labels_8)
f1(1994, race_levels_8, race_labels_8)
f1(1995, race_levels_8, race_labels_8)
f1(1996, race_levels_8, race_labels_8)
f1(1997, race_levels_8, race_labels_8)
f1(1998, race_levels_8, race_labels_8)

#1999-2002
f1(1999, race_levels_8, race_labels_8)
f1(2000, race_levels_8, race_labels_8)
f1(2001, race_levels_8, race_labels_8)
f1(2002, race_levels_8, race_labels_8)

#2003-2004
f2(2003, race_levels_8, race_labels_8)
f2(2004, race_levels_8, race_labels_8)

#2005-2018
for (year in 2005:2018) {
  f3(year, race_levels_8, race_labels_8)
}

f3(2019, race_levels_8, race_labels_8)
f3(2020, race_levels_8, race_labels_8)

race_levels_9 <- as.character(1:40)
race_labels_9 <- c(
  "White",
  "Black or African American",
  "American Indian or Alaska Native",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Asian or Pacific Islander",
  "Black or African American",
  "Black or African American",
  "Black or African American",
  "Black or African American",
  "American Indian or Alaska Native",
  "American Indian or Alaska Native",
  "American Indian or Alaska Native",
  "American Indian or Alaska Native",
  "American Indian or Alaska Native",
  "American Indian or Alaska Native",
  "Black or African American",
  "Black or African American",
  "Black or African American",
  "Black or African American",
  "Black or African American",
  "Black or African American",
  "American Indian or Alaska Native",
  "American Indian or Alaska Native",
  "American Indian or Alaska Native",
  "Asian or Pacific Islander",
  "Black or African American",
  "Black or African American",
  "Black or African American",
  "Black or African American",
  "American Indian or Alaska Native",
  "Black or African American"
)

f4(2021, race_levels_9, race_labels_9)

race_labels_10 <- c("White", "Black or African American", "American Indian or Alaska Native",
                   "Asian or Pacific Islander", "Asian or Pacific Islander")
race_levels_10 <- c(1:5)

f5(2022, race_levels_10, race_labels_10)


files_path <- "output_seasonal"

file_list <- list.files(files_path, pattern = "*.rds", full.names = TRUE)

combined_1959_1967 <- summarize_files(file_list, 1959, 1967)
saveRDS(combined_1959_1967, file.path(files_path, "Summary_1959_1967.rds"))

combined_1968_1978 <- summarize_files(file_list, 1968, 1978)
saveRDS(combined_1968_1978, file.path(files_path, "Summary_1968_1978.rds"))

combined_1979_1998 <- summarize_files(file_list, 1979, 1998)
saveRDS(combined_1979_1998, file.path(files_path, "Summary_1979_1998.rds"))

combined_1999_2022 <- summarize_files(file_list, 1999, 2022)
saveRDS(combined_1999_2022, file.path(files_path, "Summary_1999_2022.rds"))
















