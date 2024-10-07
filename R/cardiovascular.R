# run preprocessing_monthly_bycause first

library(stringr)

project_root <- here::here()
setwd(project_root)

# Load Function
source(here("R/utils_seasonal.R"))

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

all_causes_1 <- unique(cause_yearly_summary_1$cause)
matched_results_1 <- list()

for (cause in all_causes_1) {
  primary_code <- str_remove(cause, "-") %>% str_remove("^0+")
  primary_match <- icd_7 %>% filter(Code == primary_code) %>% select(Category, Disease) %>% slice(1)

  if (nrow(primary_match) > 0) {
    matched_results_1[[cause]] <- primary_match
  } else {
    primary_code <- substr(cause, 1, nchar(cause) - 1) %>% str_remove("^0+")
    secondary_code <- paste0(primary_code, ".", substr(cause, nchar(cause), nchar(cause)))
    primary_disease <- icd_7 %>% filter(Code == primary_code) %>% select(Disease) %>% pull() %>% first()
    secondary_disease <- icd_7 %>% filter(Code == secondary_code) %>% select(Disease) %>% pull() %>% first()
    category <- icd_7 %>% filter(Code == primary_code) %>% select(Category) %>% pull() %>% first()

    if (!is.na(primary_disease) ) {
      matched_results_1[[cause]] <- data.frame(Category = category, Disease = paste0(primary_disease, "-", secondary_disease))
    } else {
      reduced_code <- substr(primary_code, 1, nchar(primary_code) - 1)  # 去掉个位
      final_match <- icd_7 %>% filter(Code == reduced_code) %>% select(Category, Disease) %>% slice(1)

      if (nrow(final_match) > 0) {
        matched_results_1[[cause]] <- final_match
      } else {
        matched_results_1[[cause]] <- data.frame(Category = NA, Disease = NA)
      }
    }
  }
}


matched_df_1 <- do.call(rbind, matched_results_1)
matched_df_1$cause <- names(matched_results_1)

cause_yearly_summary_1 <- cause_yearly_summary_1 %>%
  left_join(matched_df_1, by = "cause") %>%
  filter(cause %in% c("400-", "4010", "4013", "411-", "414-", "415-", "4200","4201","4202","4340", "4341","4342","4344",
                      "465-","4211","4213","4214","4221","431-","432-","330-","331-","332-","334-","4500","452-","4533",
                      "456-","4561","4672"))

all_causes_2 <- unique(cause_yearly_summary_2$cause)

matched_results_2 <- list()

for (cause in all_causes_2) {
  cause <- str_replace(cause, "^0+", "")
  primary_code <- cause
  primary_match <- icd_8 %>% filter(Code == primary_code) %>% select(Category, Disease) %>% slice(1)

  if (nrow(primary_match) > 0) {
    matched_results_2[[cause]] <- primary_match
  } else {
    cause_with_dot <- gsub("(\\d)(\\d)$", "\\1.\\2", cause)
    primary_code <- str_extract(cause_with_dot, "^\\d+")
    secondary_code <- substr(cause_with_dot, 1, nchar(cause_with_dot))

    primary_disease <- icd_8 %>% filter(Code == primary_code) %>% select(Disease) %>% pull() %>% first()
    secondary_disease <- icd_8 %>% filter(Code == secondary_code) %>% select(Disease) %>% pull() %>% first()
    category <- icd_8 %>% filter(Code == primary_code) %>% select(Category) %>% pull() %>% first()

    if (!is.na(primary_disease) & !is.na(secondary_disease)) {
      matched_results_2[[cause]] <- data.frame(Category = category, Disease = secondary_disease)
    } else {
      matched_results_2[[cause]] <- data.frame(Category = category, Disease = NA)
    }
  }
}

matched_df_2 <- do.call(rbind, matched_results_2)
matched_df_2$cause <- names(matched_results_2)

matched_df_2 <- matched_df_2 %>%
  select(cause, Category, Disease)

cause_yearly_summary_2 <- cause_yearly_summary_2 %>%
  mutate(cause = str_replace(cause, "^0+", ""))

cause_yearly_summary_2 <- cause_yearly_summary_2 %>%
  left_join(matched_df_2, by = "cause") %>%
  filter(cause %in% c("390", "3910", "3911", "3912", "3919", "3920", "3929","393","3940","3949", "3950","3959","3960",
                      "3969","3970","3979","398","4100","4109","4110","4119","4121","4122","4123","4124","4130","4139",
                      "426","450","420","4210","4219","422","423","4240","4241","4249","425","4270","4271","4272",
                      "4273","4274","4275","4276","4279","428","4290","4299","4300","4309","4310","4319","4320","4329",
                      "4330","4339","4340","4349","4350","4359","4360","4369","4370","4379","4380","4389","4400",
                      "4401","4402","4403","4409","4410","4411","4412","4419","442","4430","4439","4440","4444","4449",
                      "4461","4462","4465","447"))



all_causes_3 <- unique(cause_yearly_summary_3$cause)
matched_results_3 <- list()

for (cause in all_causes_3) {
  primary_match <- handle_primary_9(cause, icd_9)

  if (!is.na(primary_match$Category) & !is.na(primary_match$Disease)) {
    matched_results_3[[cause]] <- primary_match
  } else {
    cause_with_dot_1 <- gsub("(\\d)(\\d)$", "\\1.\\2", cause)
    cause_with_dot_2 <- gsub("(\\d)(\\d)(\\d)$", "\\1.\\2\\3", cause)

    secondary_match_1 <- handle_secondary_9(cause_with_dot_1, icd_9)
    secondary_match_2 <- handle_secondary_9(cause_with_dot_2, icd_9)

    if (!is.na(secondary_match_1$Category) & !is.na(secondary_match_1$Disease)) {
      matched_results_3[[cause]] <- secondary_match_1
    } else if (!is.na(secondary_match_2$Category) & !is.na(secondary_match_2$Disease)) {
      matched_results_3[[cause]] <- secondary_match_2
    } else {
      matched_results_3[[cause]] <- data.frame(Category = NA, Disease = NA)
    }
  }
}

matched_df_3 <- do.call(rbind, matched_results_3)
matched_df_3$cause <- names(matched_results_3)

cause_yearly_summary_3 <- cause_yearly_summary_3 %>%
  left_join(matched_df_3, by = "cause") %>%
  filter(cause %in% c("390", "391", "3910", "3911", "3912", "3918", "3919","392", "3920","3929","393","3940","3941","3942","3949", "3950","3951","3952","3959","396",
                      "3970","3971","3979","398","3980","3989","410","411","412","413","4140","4141","4148","4149","415","4150",
                      "4151","416","4160","4161","4168","4169","417","4170","4171","4178","4179","420","4209","421","4210","4219","422","4229","4230","4231","4232","4238","4239",
                      "4273","4274","4275","4276","4279","428","4290","4299","4300","4309","4310","4319","4320","4329",
                      "4240","4241","4242","4243","4249","4250","4251","4252","4253","4254","4254","4259",
                      "4260","4261","4262","4263","4264","4265","4266","4267","4268","4269",
                      "4270","4271","4272","4273","4274","4275","4276","4278","4279","4280","4281","4289",
                      "429","4290","4291","4292","4293","4295","4296","4298","4299","430","431","432","4320","4321","4329",
                      "4330","4331","4332","4333","4338","4339","4340","4341","4349","435","436",
                      "4370","4371","4372","4373","4374","4375","4376","4378","4379","438","4400",
                      "4401","4402","4408","4409","4410","4411","4412","4413","4414","4415","4416","4420","4421","4422","4423","4428","4429","4430","4431","4438","4439","4440","4441","4442","4448","4449",
                      "4460","4461","4462","4463","4464","4465","4466","4467",
                      "4470","4471","4472","4473","4474","4475","4476","4478","4479",
                      "4480","4481","4489"))

all_causes_4 <- unique(cause_yearly_summary_4$cause)
matched_results_4 <- list()

for (cause in all_causes_4) {
  primary_match <- handle_primary_10(cause, icd_10)

  if (!is.na(primary_match$Category) & !is.na(primary_match$Disease)) {
    matched_results_4[[cause]] <- primary_match
  } else {
    cause_with_dot_1 <- sub("(\\D*)(\\d)(\\d)$", "\\1\\2.\\3", cause)
    cause_with_dot_2 <- sub("(\\D*)(\\d)(\\d)(\\d)$", "\\1\\2.\\3\\4", cause)

    secondary_match_1 <- handle_secondary_10(cause_with_dot_1, icd_10)
    secondary_match_2 <- handle_secondary_10(cause_with_dot_2, icd_10)

    if (!is.na(secondary_match_1$Category) & !is.na(secondary_match_1$Disease)) {
      matched_results_4[[cause]] <- secondary_match_1
    } else if (!is.na(secondary_match_2$Category) & !is.na(secondary_match_2$Disease)) {
      matched_results_4[[cause]] <- secondary_match_2
    } else {
      matched_results_4[[cause]] <- data.frame(Category = NA, Disease = NA)
    }
  }
}

matched_df_4 <- do.call(rbind, matched_results_4)
matched_df_4$cause <- names(matched_results_4)

cause_yearly_summary_4 <- cause_yearly_summary_4 %>%
  left_join(matched_df_4, by = "cause")%>%
  filter(str_starts(cause, "^I[0234567]"))

combined_data <- bind_rows(cause_yearly_summary_1, cause_yearly_summary_2, cause_yearly_summary_3, cause_yearly_summary_4)

cardio_data <- combined_data %>%
  rename(deaths = total_deaths)

cardio_summary <- cardio_data %>%
  group_by(year, month) %>%
  summarize(total_deaths = sum(deaths, na.rm = TRUE),
            total_population = mean(total_population, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(death_rate = total_deaths / total_population * 100000)

cardio_rate_summary <- cardio_summary %>%
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

plot7 <- ggplot(cardio_rate_summary, aes(x = year, y = ratio)) +
  geom_point(color = "darkred", size = 2) +
  geom_text(aes(label = round(ratio, 2)), vjust = -0.5, hjust = 0.5, angle = 0, color = "black", size = 3) +
  geom_vline(aes(xintercept = year), color = "grey80", linetype = "dashed", size = 0.5, alpha = 0.5) +
  scale_y_continuous(limits = c(1.0, NA), labels = scales::number_format(accuracy = 0.1)) +
  labs(title = "Yearly Ratio of Max Death Rate to Min Death Rate (Cardiovascular Disease)",
       x = "Year",
       y = "Ratio (Max Death Rate / Min Death Rate)") +
  theme_minimal()

write.csv(cardio_rate_summary,here("analysis_s/Cardiovascular Death Rate Ratio.csv"))

cardio_year_summary <- cardio_data %>%
  group_by(year) %>%
  summarise(
    total_deaths = sum(deaths, na.rm = TRUE),
    total_death_rate = sum(death_rate, na.rm = TRUE),
    .groups = 'drop'
  )
par(mfrow = c(1, 2))
plot(cardio_year_summary$year, cardio_year_summary$total_deaths, type = "o", col = "blue",
     xlab = "Year", ylab = "Total Deaths", main = "Cardiovascular Deaths Over Time")
plot(cardio_year_summary$year, cardio_year_summary$total_death_rate, type = "o", col = "red",
     xlab = "Year", ylab = "Total Death Rate", main = "Cardiovascular Death Rate Over Time")


cardio_month <- cardio_data %>%
  group_by(year, month) %>%
  summarise(
    total_deaths = sum(deaths, na.rm = TRUE),
    total_population = mean(total_population, na.rm = TRUE),
    total_death_rate = sum(death_rate, na.rm = TRUE),
    .groups = 'drop'
  )


cardio_month_summary <- cardio_month %>%
  group_by(month) %>%
  summarise(
    mean_deaths = mean(total_deaths, na.rm = TRUE),
    mean_death_rate = mean(total_death_rate, na.rm = TRUE),
    .groups = 'drop'
  )

plot(cardio_month_summary$month, cardio_month_summary$mean_deaths, type = "o", col = "blue",
              xlab = "Month", ylab = "Month Total Deaths", main = "Cardiovascular Deaths Over Time")
plot(cardio_month_summary$month, cardio_month_summary$mean_death_rate, type = "o", col = "red",
              xlab = "Month", ylab = "Month Total Death Rate", main = "Cardiovascular Death Rate Over Time")

pdf("Analysis_s/Cardiovascular Deaths.pdf", width = 15, height = 9)
par(mfrow = c(1, 2))
plot(cardio_year_summary$year, cardio_year_summary$total_deaths, type = "o", col = "blue",
     xlab = "Year", ylab = "Total Deaths", main = "Cardiovascular Deaths Over Time")
plot(cardio_year_summary$year, cardio_year_summary$total_death_rate, type = "o", col = "red",
     xlab = "Year", ylab = "Total Death Rate", main = "Cardiovascular Death Rate Over Time")

plot(cardio_month_summary$month, cardio_month_summary$mean_deaths, type = "o", col = "blue",
     xlab = "Month", ylab = "Month Total Deaths", main = "Cardiovascular Deaths Over Time")
plot(cardio_month_summary$month, cardio_month_summary$mean_death_rate, type = "o", col = "red",
     xlab = "Month", ylab = "Month Total Death Rate", main = "Cardiovascular Death Rate Over Time")

grid.arrange(plot7, newpage = TRUE)

dev.off()

write.csv(cardio_year_summary, "analysis_s/cardiovascular_year_summary.csv")
write.csv(cardio_month_summary, "analysis_s/cardiovascular_month_summary.csv")


