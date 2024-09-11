project_root <- here::here()
setwd(project_root)
output_dir <- "output_s_c"

age_labels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
age_bins <- c(-Inf, 1, seq(5, 85, by = 5), Inf)

state_codes <- data.frame(
  Code = c(1:51, 52),
  Abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "Overseas areas")
)

f0_c <- function(year, race_levels, race_labels) {
  file_path <- file.path("data", paste0("mort", year, ".csv"))
  data <- read.csv(file_path, fileEncoding="latin1")

  summary_data <- data %>%
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
    mutate(year = year) %>%
    mutate(race = factor(race, levels = race_levels, labels = race_labels)) %>%
    arrange(desc(deaths)) %>%
    mutate(fips = as.character(fips),
           fips = paste0(substr(fips, 1, nchar(fips) - 2), "0", substr(fips, nchar(fips) - 1, nchar(fips))),
           fips = as.numeric(fips))

  summary_data <- summary_data %>%
    select(year, month, state, fips, age, sex, race, cause, deaths)

  output_rds_path <- file.path("output_s_c", paste0("summary_", year, ".rds"))
  saveRDS(summary_data, file = output_rds_path)
}


f_c <- function(year, race_levels, race_labels) {
  file_path <- file.path("data", paste0("mort", year, ".csv"))
  data <- read.csv(file_path, fileEncoding="latin1")

  summary_data <- data %>%
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
    mutate(year = year) %>%
    mutate(race = factor(race, levels = race_levels, labels = race_labels)) %>%
    arrange(desc(deaths))

  summary_data <- summary_data %>%
    select(year, month, state, fips, age, sex, race, cause, deaths)

  output_rds_path <- file.path("output_s_c", paste0("summary_", year, ".rds"))
  saveRDS(summary_data, file = output_rds_path)
}


f1_c <- function(year, race_levels, race_labels) {
  file_path <- file.path("data", paste0("mort", year, ".csv"))
  data <- read.csv(file_path, fileEncoding="latin1")
  overseas_codes <- c("PR", "VI", "GU", "AS", "MP")
  summary_data <- data %>%
    mutate(age_group = case_when(
      nchar(age) == 4 & substr(age, 1, 1) == "1" & substr(age, 2, 2) == "0" ~ as.character(cut(as.numeric(gsub("^0+", "", substr(age, 3, 4))), breaks = age_bins, labels = age_labels, right = FALSE)),
      nchar(age) == 4 & substr(age, 1, 1) == "1" & substr(age, 2, 2) == "1" ~ "85+",
      nchar(age) == 4 & substr(age, 1, 1) %in% c("2", "3", "4", "5", "6") ~ "<1",
      TRUE ~ "999"
    )) %>%
    select(monthdth, sex, age_group, race, stateoc, countyoc, ucod) %>%
    rename(cause = ucod, age = age_group, month = monthdth, fips = countyoc, state = stateoc) %>%
    mutate(state = ifelse(state %in% overseas_codes, "Overseas areas", state)) %>%
    mutate(sex = ifelse(sex == "M", "Male", "Female")) %>%
    mutate(state_abbrev = substr(fips, 1, 2)) %>%
    left_join(state_codes, by = c("state_abbrev" = "Abbrev")) %>%
    mutate(fips = paste0(Code, substr(fips, 3, nchar(fips)))) %>%
    select(-state_abbrev, -Code) %>%
    group_by(month, state, fips, age, sex, race, cause) %>%
    summarise(deaths = n(), .groups = 'drop') %>%
    mutate(year = year) %>%
    mutate(race = factor(race, levels = race_levels, labels = race_labels)) %>%
    arrange(desc(deaths))

  summary_data <- summary_data %>%
    select(year, month, state, fips, age, sex, race, cause, deaths)

  output_rds_path <- file.path("output_s_c", paste0("summary_", year, ".rds"))
  saveRDS(summary_data, file = output_rds_path)
}

handle_primary_7 <- function(cause, icd) {
  primary_code <- str_remove(cause, "-") %>% str_remove("^0+")
  disease_info <- icd %>% filter(Code == primary_code) %>% select(Category, Disease) %>% slice(1)
  if (nrow(disease_info) > 0) {
    return(disease_info)
  }
  return(data.frame(Category = NA, Disease = NA))
}

handle_secondary_7 <- function(cause, icd) {
  primary_code <- substr(cause, 1, nchar(cause) - 1) %>% str_remove("^0+")
  secondary_code <- paste0(primary_code, ".", substr(cause, nchar(cause), nchar(cause)))
  primary_disease <- icd %>% filter(Code == primary_code) %>% select(Disease) %>% pull() %>% first()
  secondary_disease <- icd %>% filter(Code == secondary_code) %>% select(Disease) %>% pull() %>% first()
  category <- icd %>% filter(Code == primary_code) %>% select(Category) %>% pull() %>% first()
  if (!is.na(primary_disease) & !is.na(secondary_disease)) {
    return(data.frame(Category = category, Disease = paste0(primary_disease, "-", secondary_disease)))
  }
  return(data.frame(Category = category, Disease = NA))
}

process_data_for_years_7 <- function(data, start_year, end_year, icd) {
  data_filtered <- data %>%
    filter(year >= start_year & year <= end_year) %>%
    rowwise() %>%
    mutate(disease_info = case_when(
      str_detect(cause, "-") ~ list(handle_primary_7(cause, icd)),
      TRUE ~ list(handle_secondary_7(cause, icd))
    )) %>%
    mutate(Disease = disease_info$Disease, Category = disease_info$Category) %>%
    ungroup() %>%
    select(-disease_info)

  return(data_filtered)
}

handle_primary_8 <- function(cause, icd) {
  primary_code <- cause
  disease_info <- icd %>% filter(Code == primary_code) %>% select(Category, Disease) %>% slice(1)
  if (nrow(disease_info) > 0) {
    return(disease_info)
  }
  return(data.frame(Category = NA, Disease = NA))
}

handle_secondary_8 <- function(cause, icd) {
  cause_with_dot <- gsub("(\\d)(\\d)$", "\\1.\\2", cause)
  primary_code <- str_extract(cause_with_dot, "^\\d+")
  secondary_code <- substr(cause_with_dot, 1, nchar(cause_with_dot))
  primary_disease <- icd %>% filter(Code == primary_code) %>% select(Disease) %>% pull() %>% first()
  secondary_disease <- icd %>% filter(Code == secondary_code) %>% select(Disease) %>% pull() %>% first()
  category <- icd %>% filter(Code == primary_code) %>% select(Category) %>% pull() %>% first()
  if (!is.na(primary_disease) & !is.na(secondary_disease)) {
    return(data.frame(Category = category, Disease = paste0(primary_disease, "-", secondary_disease)))
  }
  return(data.frame(Category = category, Disease = NA))
}

process_data_for_years_8 <- function(data, start_year, end_year, icd) {
  data_filtered <- data %>%
    filter(year >= start_year & year <= end_year) %>%
    rowwise() %>%
    mutate(disease_info = case_when(
      cause %in% icd$Code ~ list(handle_primary_8(cause, icd)),
      TRUE ~ list(handle_secondary_8(cause, icd))
    )) %>%
    mutate(Disease = disease_info$Disease, Category = disease_info$Category) %>%
    ungroup() %>%
    select(-disease_info)

  return(data_filtered)
}

handle_primary_9 <- function(cause, icd) {
  primary_code <- cause
  disease_info <- icd %>% filter(Code == primary_code) %>% select(Category, Disease) %>% slice(1)
  if (nrow(disease_info) > 0) {
    return(disease_info)
  }
  return(data.frame(Category = NA, Disease = NA))
}

handle_secondary_9 <- function(cause, icd) {
  primary_code <- str_extract(cause, "^\\d+")
  secondary_code <- substr(cause, 1, nchar(cause))
  primary_disease <- icd %>% filter(Code == primary_code) %>% select(Disease) %>% pull() %>% first()
  secondary_disease <- icd %>% filter(Code == secondary_code) %>% select(Disease) %>% pull() %>% first()
  category <- icd %>% filter(Code == primary_code) %>% select(Category) %>% pull() %>% first()
  if (!is.na(primary_disease) & !is.na(secondary_disease)) {
    return(data.frame(Category = category, Disease = paste0(primary_disease, "-", secondary_disease)))
  }
  return(data.frame(Category = category, Disease = NA))
}

process_data_for_years_9 <- function(data, start_year, end_year, icd) {
  data_filtered <- data %>%
    filter(year >= start_year & year <= end_year) %>%
    rowwise() %>%
    mutate(cause_with_dot_1 = gsub("(\\d)(\\d)$", "\\1.\\2", cause),
           cause_with_dot_2 = gsub("(\\d)(\\d)(\\d)$", "\\1.\\2\\3", cause)) %>%
    mutate(disease_info = case_when(
      cause %in% icd$Code ~ list(handle_primary_9(cause, icd)),
      cause_with_dot_1 %in% icd$Code ~ list(handle_secondary_9(cause_with_dot_1, icd)),
      cause_with_dot_2 %in% icd$Code ~ list(handle_secondary_9(cause_with_dot_2, icd)))) %>%
    mutate(Disease = disease_info$Disease, Category = disease_info$Category) %>%
    ungroup() %>%
    select(-c(disease_info, cause_with_dot_1, cause_with_dot_2))

  return(data_filtered)
}

handle_primary_10 <- function(cause, icd) {
  primary_code <- cause
  disease_info <- icd %>% filter(Code == primary_code) %>% select(Category, Disease) %>% slice(1)
  if (nrow(disease_info) > 0) {
    return(disease_info)
  }
  return(data.frame(Category = NA, Disease = NA))
}

handle_secondary_10 <- function(cause, icd) {
  cause_with_dot <- sub("(\\D*)(\\d)(\\d)$", "\\1\\2.\\3", cause)
  primary_code <- str_extract(cause_with_dot, "^\\D*\\d+\\.\\d")
  secondary_code <- substr(cause_with_dot, 1, nchar(cause_with_dot))

  primary_disease <- icd %>% filter(Code == primary_code) %>% select(Disease) %>% pull() %>% first()
  secondary_disease <- icd %>% filter(Code == secondary_code) %>% select(Disease) %>% pull() %>% first()
  category <- icd %>% filter(Code == primary_code) %>% select(Category) %>% pull() %>% first()

  if (!is.na(secondary_disease)) {
    return(data.frame(Category = category, Disease = secondary_disease))
  }
  return(data.frame(Category = category, Disease = NA))
}

process_data_for_years_10 <- function(data, start_year, end_year, icd) {
  data_filtered <- data %>%
    filter(year >= start_year & year <= end_year) %>%
    rowwise() %>%
    mutate(disease_info = case_when(
      cause %in% icd$Code ~ list(handle_primary_10(cause, icd)),
      TRUE ~ list(handle_secondary_10(cause, icd))
    )) %>%
    mutate(Disease = disease_info$Disease, Category = disease_info$Category) %>%
    ungroup() %>%
    select(-disease_info)

  return(data_filtered)
}

