library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(patchwork)
library(grid)
library(here)

project_root <- here::here()
setwd(project_root)

get_days_in_month <- function(year, month) {
  if (month == 2) {
    if (year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0)) {
      return(29)
    } else {
      return(28)
    }
  } else if (month %in% c(4, 6, 9, 11)) {
    return(30)
  } else {
    return(31)
  }
}

population <- read.table("data/resident_population.csv", sep = ",", header = TRUE)
population <- population %>%
  rename(year = Years, population = Resident.Population..People.)

data_1959_1967 <- read_rds("output_seasonal/Summary_1959_1967.rds")

data_1968_1978 <- read_rds("output_seasonal/Summary_1968_1978.rds")

data_1979_1998 <- read_rds("output_seasonal/Summary_1979_1998.rds")

data_1999_2022 <- read_rds("output_seasonal/Summary_1999_2022.rds")

cause_summarized_1959_1967 <- data_1959_1967 %>%
  select(-state, -fips, -age, -sex, -race) %>%
  group_by(year, month, cause) %>%
  summarize(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop_last") %>%
  rename(deaths = total_deaths)

cause_summarized_1968_1978 <- data_1968_1978 %>%
  select(-state, -fips, -age, -sex, -race) %>%
  group_by(year, month, cause) %>%
  summarize(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop_last") %>%
  rename(deaths = total_deaths)

cause_summarized_1979_1998 <- data_1979_1998 %>%
  select(-state, -fips, -age, -sex, -race) %>%
  group_by(year, month, cause) %>%
  summarize(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop_last") %>%
  rename(deaths = total_deaths)

cause_summarized_1999_2022 <- data_1999_2022 %>%
  select(-state, -fips, -age, -sex, -race) %>%
  group_by(year, month, cause) %>%
  summarize(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop_last") %>%
  rename(deaths = total_deaths)

month_average_data_1959_1967 <- cause_summarized_1959_1967 %>%
  mutate(days_in_month = mapply(get_days_in_month, year, month))

month_average_data_1968_1978 <- cause_summarized_1968_1978 %>%
  mutate(days_in_month = mapply(get_days_in_month, year, month))

month_average_data_1979_1998 <- cause_summarized_1979_1998 %>%
  mutate(days_in_month = mapply(get_days_in_month, year, month))

month_average_data_1999_2022 <- cause_summarized_1999_2022 %>%
  mutate(days_in_month = mapply(get_days_in_month, year, month))

monthly_unified_1959_1967 <- month_average_data_1959_1967 %>%
  group_by(year,month,cause) %>%
  summarise(total_deaths = sum(deaths),
            days_in_month = first(days_in_month), .groups = 'drop') %>%
  mutate(average_deaths_per_day = total_deaths / days_in_month,
         deaths_estimate = average_deaths_per_day * 30) %>%
  select(-total_deaths, -days_in_month, -average_deaths_per_day) %>%
  rename(deaths = deaths_estimate) %>%
  arrange(year, desc(deaths))

monthly_unified_1968_1978 <- month_average_data_1968_1978 %>%
  group_by(year,month,cause) %>%
  summarise(total_deaths = sum(deaths),
            days_in_month = first(days_in_month), .groups = 'drop') %>%
  mutate(average_deaths_per_day = total_deaths / days_in_month,
         deaths_estimate = average_deaths_per_day * 30) %>%
  select(-total_deaths, -days_in_month, -average_deaths_per_day) %>%
  rename(deaths = deaths_estimate) %>%
  arrange(year, desc(deaths))

monthly_unified_1979_1998 <- month_average_data_1979_1998 %>%
  group_by(year,month,cause) %>%
  summarise(total_deaths = sum(deaths),
            days_in_month = first(days_in_month), .groups = 'drop') %>%
  mutate(average_deaths_per_day = total_deaths / days_in_month,
         deaths_estimate = average_deaths_per_day * 30) %>%
  select(-total_deaths, -days_in_month, -average_deaths_per_day) %>%
  rename(deaths = deaths_estimate) %>%
  arrange(year, desc(deaths))

monthly_unified_1999_2022 <- month_average_data_1999_2022 %>%
  group_by(year,month,cause) %>%
  summarise(total_deaths = sum(deaths),
            days_in_month = first(days_in_month), .groups = 'drop') %>%
  mutate(average_deaths_per_day = total_deaths / days_in_month,
         deaths_estimate = average_deaths_per_day * 30) %>%
  select(-total_deaths, -days_in_month, -average_deaths_per_day) %>%
  rename(deaths = deaths_estimate) %>%
  arrange(year, desc(deaths))

data_unified_1959_1967 <- merge(monthly_unified_1959_1967, population, by = "year")

data_unified_1968_1978 <- merge(monthly_unified_1968_1978, population, by = "year")

data_unified_1979_1998 <- merge(monthly_unified_1979_1998, population, by = "year")

data_unified_1999_2022 <- merge(monthly_unified_1999_2022, population, by = "year")

data_unified <- bind_rows(
  data_unified_1959_1967,
  data_unified_1968_1978,
  data_unified_1979_1998,
  data_unified_1999_2022
)

# Calculate monthly death rate for each year, regardless of other factors
yearly_summary <- data_unified %>%
  group_by(year, month) %>%
  summarise(
    total_deaths = sum(deaths),
    total_population = mean(population),
    .groups = 'drop'
  ) %>%
  mutate(death_rate = (total_deaths / total_population) * 100000)

yearly_rate_summary <- yearly_summary %>%
  group_by(year) %>%
  summarise(
    population = mean(total_population),
    min_month = month[which.min(death_rate)],
    min_death_rate = min(death_rate),
    max_month = month[which.max(death_rate)],
    max_death_rate = max(death_rate),
    ratio = max_death_rate / min_death_rate,
    .groups = 'drop'
  )

# Calculate monthly death rate for each year, by cause
cause_yearly_summary_1 <- data_unified_1959_1967 %>%
  group_by(cause, year, month) %>%
  summarise(
    total_deaths = sum(deaths),
    total_population = mean(population),
    .groups = 'drop'
  ) %>%
  mutate(death_rate = (total_deaths / total_population) * 100000)

cause_monthly_summary_1 <- cause_yearly_summary_1 %>%
  group_by(cause, month) %>%
  summarise(
    mean_deaths = mean(total_deaths),
    mean_death_rate = mean(death_rate),
    .groups = 'drop'
  )

cause_yearly_summary_2 <- data_unified_1968_1978 %>%
  group_by(cause, year, month) %>%
  summarise(
    total_deaths = sum(deaths),
    total_population = mean(population),
    .groups = 'drop'
  ) %>%
  mutate(death_rate = (total_deaths / total_population) * 100000)

cause_monthly_summary_2 <- cause_yearly_summary_2 %>%
  group_by(cause, month) %>%
  summarise(
    mean_deaths = mean(total_deaths),
    mean_death_rate = mean(death_rate),
    .groups = 'drop'
  )

cause_yearly_summary_3 <- data_unified_1979_1998 %>%
  group_by(cause, year, month) %>%
  summarise(
    total_deaths = sum(deaths),
    total_population = mean(population),
    .groups = 'drop'
  ) %>%
  mutate(death_rate = (total_deaths / total_population) * 100000)

cause_monthly_summary_3 <- cause_yearly_summary_3 %>%
  group_by(cause, month) %>%
  summarise(
    mean_deaths = mean(total_deaths),
    mean_death_rate = mean(death_rate),
    .groups = 'drop'
  )

cause_yearly_summary_4 <- data_unified_1999_2022 %>%
  group_by(cause, year, month) %>%
  summarise(
    total_deaths = sum(deaths),
    total_population = mean(population),
    .groups = 'drop'
  ) %>%
  mutate(death_rate = (total_deaths / total_population) * 100000)

cause_monthly_summary_4 <- cause_yearly_summary_4 %>%
  group_by(cause, month) %>%
  summarise(
    mean_deaths = mean(total_deaths),
    mean_death_rate = mean(death_rate),
    .groups = 'drop'
  )