library(dplyr)
library(ggplot2)
library(ggrepel)

setwd("/Users/yuanyulu/nber_mortality_process_2024")
data <- read.csv("data/mort1970.csv")

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

sprintf("%.2f%%", 100*sum(compare_data$unmatching_deaths)/sum(compare_data$deaths))

mismatch_cause <- data %>%
  filter(countyoc != countyrs) %>%
  select(ucod) %>%
  rename(cause = ucod) %>%
  group_by(cause) %>%
  summarise(deaths = n(), .groups = 'drop')

top10_cause <- mismatch_cause %>%
  top_n(10, deaths) %>%
  arrange(desc(deaths))

ggplot(top10_cause, aes(x = reorder(cause, deaths), y = deaths, fill = cause)) +
  geom_col() +
  labs(title = "Top 10 Causes of Death with Mismatched County Information",
       x = "Cause of Death",
       y = "Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mismatch_age <- data %>%
  filter(countyoc != countyrs) %>%
  select(age) %>%
  group_by(age) %>%
  summarise(deaths = n(), .groups = 'drop') %>%
  filter(age<105)

ggplot(mismatch_age, aes(x = age, y = deaths)) +
  geom_line() +
  labs(title = "Deaths by Age for Mismatched County Data",
       x = "Age",
       y = "Number of Deaths") +
  theme_minimal()

ggplot(mismatch_age, aes(x = age, y = deaths)) +
  scale_x_continuous(limits = c(60, 90)) +
  scale_y_continuous(limits = c(5000, 17000)) +
  geom_line() +
  geom_point(color = "red") +
  geom_text_repel(aes(label = deaths),
                  color = "darkgreen",
                  size = 3,
                  box.padding = 0.35,
                  point.padding = 0.5,
                  max.overlaps = 10) +
  labs(title = "Deaths by Age for Mismatched County Data",
       x = "Age",
       y = "Number of Deaths") +
  theme_minimal()