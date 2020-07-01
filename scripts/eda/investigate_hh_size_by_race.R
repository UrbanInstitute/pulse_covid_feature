library(tidyverse)

# Every row in the puf file is a respondent in a given week
puf_all_weeks <- read_csv("data/intermediate-data/pulse_puf_all_weeks.csv")

puf_unique_respondents <- puf_all_weeks %>% distinct(scram, week_num, .keep_all = T)

hhld_size_by_race <- puf_unique_respondents %>%
  group_by(week_num, hisp_rrace) %>%
  summarize(mean_hhld_size = mean(thhld_numper))


hhld_size_by_state <- puf_unique_respondents %>%
  group_by(week_num, state_name) %>%
  summarize(mean_hhld_size = mean(thhld_numper)) %>%
  arrange(week_num, mean_hhld_size)

hhld_size_by_msa <- puf_unique_respondents %>%
  group_by(week_num, `CSA Title`) %>%
  summarize(mean_hhld_size = mean(thhld_numper)) %>%
  arrange(week_num, mean_hhld_size)


hhld_size_by_race_and_state <- puf_unique_respondents %>%
  group_by(week_num, state_name, hisp_rrace) %>%
  summarize(mean_hhld_size = mean(thhld_numper)) %>%
  arrange(week_num, mean_hhld_size)


hhld_size_by_race_and_msa <- puf_unique_respondents %>%
  group_by(week_num, `CSA Title`, hisp_rrace) %>%
  summarize(mean_hhld_size = mean(thhld_numper)) %>%
  arrange(week_num, mean_hhld_size)


# Household size by race
hh_size_by_race_plot <- ggplot(hhld_size_by_race, aes(
  x = week_num,
  y = mean_hhld_size,
  group = hisp_rrace,
  color = hisp_rrace
)) +
  geom_point() +
  geom_line()

dir.create("output/eda_standard_errors", recursive = T, showWarnings = FALSE)
ggsave("output/eda_standard_errors/hh_size_by_race.png")

# Household size by race and State
hh_size_by_race_state_plot <- ggplot(hhld_size_by_race_and_state, aes(
  x = week_num,
  y = mean_hhld_size,
  group = hisp_rrace,
  color = hisp_rrace
)) +
  geom_point() +
  geom_line() +
  facet_wrap(~state_name)

ggsave("output/eda_standard_errors/hh_size_by_race_state.png")

# Household size by race and MSA
hh_size_by_race_msa_plot <- ggplot(hhld_size_by_race_and_msa, aes(
  x = week_num,
  y = mean_hhld_size,
  group = hisp_rrace,
  color = hisp_rrace
)) +
  geom_point() +
  geom_line() +
  facet_wrap(~`CSA Title`)

ggsave("output/eda_standard_errors/hh_size_by_race_msa.png")
