library(dplyr)
library(ggplot2)
library(gridExtra)


project_root <- here::here()
setwd(project_root)

age_levels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                "60-64", "65-69", "70-74", "75-79", "80-84", "85+", "999")

state_labels <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "Overseas areas")


#unkown age sex race each year
missing_data_per_year <- data.frame(
  year = integer(),
  age_not_stated = integer(),
  sex_not_stated = integer(),
  race_not_stated = integer(),
  race_not_specified = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  age_not_stated <- summary_data %>%
    filter(age == "999") %>%
    summarise(count = n()) %>%
    pull(count)

  sex_not_stated <- summary_data %>%
    filter(is.na(sex)) %>%
    summarise(count = n()) %>%
    pull(count)

  race_not_stated <- summary_data %>%
    filter(is.na(race) | grepl("not stated", race, ignore.case = TRUE)) %>%
    summarise(count = n()) %>%
    pull(count)

  race_not_specified <- summary_data %>%
    filter(grepl("other", race, ignore.case = TRUE)) %>%
    summarise(count = n()) %>%
    pull(count)

  missing_data <- data.frame(
    year = year,
    age_not_stated = age_not_stated,
    sex_not_stated = sex_not_stated,
    race_not_stated = race_not_stated,
    race_not_specified = race_not_specified
  )

  missing_data_per_year <- bind_rows(missing_data_per_year, missing_data)
}

print(missing_data_per_year)

write.csv(missing_data_per_year,"Output/missingdata.csv",row.names = FALSE)


# total deaths each year
total_deaths_per_year <- data.frame(
  year = integer(),
  total_deaths = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  total_deaths <- summary_data %>%
    summarise(total_deaths = sum(deaths)) %>%
    mutate(year = year)

  total_deaths_per_year <- bind_rows(total_deaths_per_year, total_deaths)
}

plot1 <- ggplot(total_deaths_per_year, aes(x = year, y = total_deaths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Deaths per Year from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal()


#total deaths each year by state
total_deaths_per_year_state <- data.frame(
  year = integer(),
  state = factor(levels = state_labels),
  total_deaths = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  total_deaths <- summary_data %>%
    group_by(state) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(year = year, state = factor(state, levels = state_labels))

  total_deaths_per_year_state <- bind_rows(total_deaths_per_year_state, total_deaths)
}

plot2 <- ggplot(total_deaths_per_year_state, aes(x = factor(year), y = total_deaths, fill = state)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by State from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ state, scales = "free_y")

plot3 <- ggplot(total_deaths_per_year_state, aes(x = factor(year), y = total_deaths)) +
  geom_point(aes(color = state)) +
  labs(title = "Total Deaths per Year by State from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ state, scales = "free_y")

#total deaths each year by age
total_deaths_per_year_age_group <- data.frame(
  year = integer(),
  age = factor(levels = age_levels),
  total_deaths = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  total_deaths <- summary_data %>%
    group_by(age) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(year = year, age = factor(age, levels = age_levels))

  total_deaths_per_year_age_group <- bind_rows(total_deaths_per_year_age_group, total_deaths)
}

plot4 <- ggplot(total_deaths_per_year_age_group, aes(x = factor(year), y = total_deaths, fill = age)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by Age Group from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ age, scales = "free_y")


#total deaths each year by sex
total_deaths_per_year_sex <- data.frame(
  year = integer(),
  sex = character(),
  total_deaths = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  total_deaths <- summary_data %>%
    group_by(sex) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(year = year)

  total_deaths_per_year_sex <- bind_rows(total_deaths_per_year_sex, total_deaths)
}

plot5 <- ggplot(total_deaths_per_year_sex, aes(x = factor(year), y = total_deaths, fill = sex)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by Sex from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ sex, scales = "free_y")

#total deaths each year by sex and age
total_deaths_per_year_age_sex <- data.frame(
  year = integer(),
  sex = character(),
  age = factor(levels = age_labels),
  total_deaths = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  total_deaths <- summary_data %>%
    group_by(sex, age) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(year = year, age = factor(age, levels = age_labels))  # 转换为有序因子

  total_deaths_per_year_age_sex <- bind_rows(total_deaths_per_year_age_sex, total_deaths)
}

plot6 <- ggplot(total_deaths_per_year_age_sex %>% filter(sex == "Female"), aes(x = factor(year), y = total_deaths, fill = age)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by Age Group (Female) from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ age, scales = "free_y")

plot7 <- ggplot(total_deaths_per_year_age_sex %>% filter(sex == "Male"), aes(x = factor(year), y = total_deaths, fill = age)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by Age Group (Male) from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ age, scales = "free_y")

#total deaths each year by race
total_deaths_per_year_race <- data.frame(
  year = integer(),
  race = character(),
  total_deaths = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  total_deaths <- summary_data %>%
    group_by(race) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(year = year)

  total_deaths_per_year_race <- bind_rows(total_deaths_per_year_race, total_deaths)
}

plot8 <- ggplot(total_deaths_per_year_race, aes(x = factor(year), y = total_deaths, fill = race)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by Race from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ race, scales = "free_y")

total_deaths_per_year_race_unified <- data.frame(
  year = integer(),
  race = character(),
  total_deaths = integer()
)

selected_races <- c("White", "Black", "Chinese", "Japanese", "Filipino")

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  processed_data <- summary_data %>%
    mutate(race = case_when(
      race %in% selected_races ~ race,
      grepl("Indian|Aleuts|Eskimos", race, ignore.case = TRUE) ~ "American Indian (includes Aleuts and Eskimos)",
      grepl("Hawaiian", race, ignore.case = TRUE) ~ "Hawaiian (includes part-Hawaiian)",
      grepl("Race not stated", race, ignore.case = TRUE) ~ "Race not stated",
      TRUE ~ "All other races"
    ))

  total_deaths <- processed_data %>%
    group_by(race, year) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop')

  total_deaths_per_year_race_unified <- bind_rows(total_deaths_per_year_race_unified, total_deaths)
}

plot9 <- ggplot(total_deaths_per_year_race_unified, aes(x = factor(year), y = total_deaths, fill = race)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by Unified Race from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ race, scales = "free_y")

# Asian Records State Distribution
chinese_distribution <- data.frame(
  year = integer(),
  state = factor(levels = state_labels),
  total_deaths = integer()
)

japanese_distribution <- data.frame(
  year = integer(),
  state = factor(levels = state_labels),
  total_deaths = integer()
)

filipino_distribution <- data.frame(
  year = integer(),
  state = factor(levels = state_labels),
  total_deaths = integer()
)

for (year in 1959:2004) {
  file_path <- file.path("output", paste0("summary_", year, ".rds"))
  summary_data <- readRDS(file_path)

  chinese_data <- summary_data %>%
    filter(race == "Chinese") %>%
    group_by(state, year) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(state = factor(state, levels = state_labels))

  japanese_data <- summary_data %>%
    filter(race == "Japanese") %>%
    group_by(state, year) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(state = factor(state, levels = state_labels))

  filipino_data <- summary_data %>%
    filter(race == "Filipino") %>%
    group_by(state, year) %>%
    summarise(total_deaths = sum(deaths), .groups = 'drop') %>%
    mutate(state = factor(state, levels = state_labels))

  chinese_distribution <- bind_rows(chinese_distribution, chinese_data)
  japanese_distribution <- bind_rows(japanese_distribution, japanese_data)
  filipino_distribution <- bind_rows(filipino_distribution, filipino_data)
}

plot10 <- ggplot(chinese_distribution, aes(x = factor(year), y = total_deaths, fill = state)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by State for Chinese from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ state, scales = "free_y")

plot11 <- ggplot(japanese_distribution, aes(x = factor(year), y = total_deaths, fill = state)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by State for Japanese from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ state, scales = "free_y")

plot12 <- ggplot(filipino_distribution, aes(x = factor(year), y = total_deaths, fill = state)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Deaths per Year by State for Filipino from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ state, scales = "free_y")

#Cause
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
    mutate(year = year, cause = as.character(cause)) %>%
    arrange(desc(total_deaths)) %>%
    slice_head(n = 50)

  total_deaths_per_year_cause <- bind_rows(total_deaths_per_year_cause, total_deaths)
}

plot13 <- ggplot(total_deaths_per_year_cause, aes(x = reorder(cause, -total_deaths), y = total_deaths, fill = cause)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 50 Causes of Death per Year from 1959 to 2004",
       x = "Cause",
       y = "Total Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  facet_wrap(~ year, scales = "free_y", ncol = 5)


plot14 <- ggplot(summary_deaths_per_year_cause, aes(x = factor(year), y = total_deaths)) +
  geom_col(fill = "steelblue") +
  labs(title = "Annual Deaths by Category from 1959 to 2004",
       x = "Year",
       y = "Total Deaths") +
  theme_minimal() +
  coord_cartesian(ylim = c(3000, NA)) +
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        strip.text.x = element_text(hjust = 0, size = 10)) +
  facet_wrap(~ Category, scales = "free_y", ncol = 6)

print(plot14)

pdf("Analysis/Summary Plots.pdf", width = 15, height = 9)

grid.arrange(plot1)
grid.arrange(plot2)
grid.arrange(plot3)
grid.arrange(plot4)
grid.arrange(plot5)
grid.arrange(plot6)
grid.arrange(plot7)
grid.arrange(plot8)
grid.arrange(plot9)
grid.arrange(plot10)
grid.arrange(plot11)
grid.arrange(plot12)
grid.arrange(plot13)
grid.arrange(plot14)

dev.off()