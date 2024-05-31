library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)

setwd("/Users/yuanyulu/nber_mortality_process_2024")
data <- read.csv("data/mort1970.csv")

#summary
summary_data <- data %>%
  select(ucod, monthdth, sex, age, countyoc) %>%
  rename(cause = ucod, fips = countyoc) %>%
  group_by(cause, monthdth, sex, age, fips) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  mutate(year = 1970)

output_dir <- "output"
output_path <- file.path(output_dir, "summary_1970_data.csv")

write.csv(summary_data, output_path, row.names = FALSE)

compare_data <- data %>%
  select(ucod, monthdth, sex, age, countyoc, countyrs) %>%
  rename(cause = ucod, fips = countyoc) %>%
  mutate(match = fips == countyrs) %>%
  group_by(cause, monthdth, sex, age, fips) %>%
  summarise(
    deaths = n(),
    matching_deaths = sum(match, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    unmatching_deaths = deaths - matching_deaths,
    unmatching_ratio = unmatching_deaths / deaths,
    year = 1970
  ) %>%
  select(-matching_deaths)

overall_data <- data.frame(
  category = c("Match", "Mismatch"),
  count = c(sum(compare_data$deaths) - sum(compare_data$unmatching_deaths), sum(compare_data$unmatching_deaths)),
  percentage = c(1 - overall_mismatch_ratio, overall_mismatch_ratio)
)

ggplot(overall_data, aes(x = "", y = count, fill = category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Mismatch Ratio in Total Deaths") +
  geom_text(aes(label = scales::percent(percentage)),
            position = position_stack(vjust = 0.5))

fips_mismatch_ratio <- data %>%
  select(ucod, monthdth, sex, age, countyoc, countyrs) %>%
  mutate(countyrs = if_else(substr(countyrs, 1, 1) == "0" & nchar(countyrs) == 5,
                            gsub("^0", "", countyrs), countyrs)) %>%
  mutate(match = countyoc == countyrs) %>%
  group_by(countyoc) %>%
  summarise(
    total_deaths = n(),
    matching_deaths = sum(match, na.rm = TRUE),
    unmatching_deaths = total_deaths - matching_deaths,
    ratio_within = scales::percent(unmatching_deaths / total_deaths),
    .groups = 'drop'
  ) %>%
  filter(unmatching_deaths > 0) %>%
  mutate(ratio = scales::percent(unmatching_deaths / sum(unmatching_deaths))) %>%
  arrange(desc(unmatching_deaths))



age_labels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
age_bins <- c(-Inf, 1, seq(5, 85, by = 5), Inf)

data_byage <- data %>%
  filter(age<105) %>%
  mutate(age_group = cut(age, breaks = age_bins, labels = age_labels, right = FALSE)) %>%
  select(-age) %>%
  rename(age=age_group)

summary_agegroup <- data_byage %>%
  select(ucod, monthdth, sex, age, countyoc) %>%
  rename(cause = ucod, fips = countyoc) %>%
  group_by(cause, monthdth, sex, age, fips) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  mutate(year = 1970) %>%
  arrange(desc(deaths))
print(summary_agegroup)

# cause summary
summary_cause <- data %>%
  select(ucod) %>%
  rename(cause = ucod) %>%
  group_by(cause) %>%
  summarise(deaths = n(), .groups = 'drop')

top10_cause <- summary_cause %>%
  top_n(10, deaths) %>%
  arrange(desc(deaths))
print(top10_cause)

ggplot(top10_cause, aes(x = reorder(cause, deaths), y = deaths)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = deaths), vjust = -0.3, color = "black", size = 3.5) +
  labs(title = "Top 10 Causes of Death in 1970",
       x = "Cause of Death",
       y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#unmatching county & cause
mismatch_cause <- data %>%
  filter(countyoc != countyrs) %>%
  select(ucod) %>%
  rename(cause = ucod) %>%
  group_by(cause) %>%
  summarise(deaths = n(), .groups = 'drop')

top10_mismatch_cause <- mismatch_cause %>%
  top_n(10, deaths) %>%
  arrange(desc(deaths))

causes <- data.frame(total=top10_cause$cause,mismatch=top10_mismatch_cause$cause)

#summary by age group
age_labels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
age_bins <- c(-Inf, 1, seq(5, 85, by = 5), Inf)

data_byage <- data %>%
  filter(age<105) %>%
  mutate(age_group = cut(age, breaks = age_bins, labels = age_labels, right = FALSE)) %>%
  select(-age) %>%
  rename(age=age_group)

summary_agegroup <- data_byage %>%
  select(ucod, monthdth, sex, age, countyoc) %>%
  rename(cause = ucod, fips = countyoc) %>%
  group_by(cause, monthdth, sex, age, fips) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  mutate(year = 1970) %>%
  arrange(desc(deaths))
print(summary_agegroup_clean)

output_path <- file.path(output_dir, "summary_1970_data_byage.csv")
write.csv(summary_agegroup, output_path, row.names = FALSE)

summary_agegroup_new <- summary_agegroup %>%
  group_by(age) %>%
  summarise(deaths = n(), .groups = 'drop')

ggplot(summary_agegroup_new, aes(x = age, y = deaths)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = deaths), vjust = -0.3, color = "black", size = 3.5) +
  labs(title = "Deaths by Age Group for Mismatched County Data",
       x = "Age Group",
       y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#unmatching county & age
mismatch_age <- data %>%
  filter(countyoc != countyrs) %>%
  filter(age<105) %>%
  mutate(age_group = cut(age, breaks = age_bins, labels = age_labels, right = FALSE)) %>%
  group_by(age_group) %>%
  summarise(deaths = n(), .groups = 'drop')

ggplot(mismatch_age, aes(x = age_group, y = deaths)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = deaths), vjust = -0.3, color = "black", size = 3.5) +
  labs(title = "Deaths by Age Group for Mismatched County Data",
       x = "Age Group",
       y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#race
race_labels <- c("Guamian", "White", "Black", "Indian (includes Aleuts and Eskimos)",
                 "Chinese", "Japanese", "Hawaiian (includes part-Hawaiian)",
                 "All other races", "Filipino")

summary_race <- data %>%
  select(ucod, monthdth, sex, age, race, countyoc) %>%
  filter(age<105) %>%
  rename(cause = ucod, fips = countyoc) %>%
  group_by(cause, monthdth, sex, age, fips, race) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  mutate(year = 1970) %>%
  mutate(race = factor(race, levels = 0:8, labels = race_labels)) %>%
  arrange(desc(deaths))

average_age_by_race <- summary_race %>%
  group_by(race) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), .groups = 'drop')

ggplot(summary_race, aes(x = factor(race), y = age)) +
  geom_violin() +
  scale_x_discrete(labels = race_labels) +
  labs(title = "Death Age Distribution by Race",
       x = "Race",
       y = "Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#unmatching county & race
mismatch_race <- data %>%
  filter(countyoc != countyrs) %>%
  mutate(race = factor(race, levels = 0:8, labels = race_labels)) %>%
  group_by(race) %>%
  summarise(deaths = n(), .groups = 'drop')

ggplot(mismatch_race, aes(x = race, y = deaths)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = deaths), vjust = -0.3, color = "black", size = 3.5) +
  labs(title = "Deaths by Race Group for Mismatched County Data",
       x = "Race Group",
       y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

