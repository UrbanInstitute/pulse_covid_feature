library(tidyverse)
library(here)

all_data <- read.csv(here("data/final-data", "rolling_all_to_current_week.csv"))
spend_vars <- read.csv(here('data/final-data', "spend_vars_to_current_week.csv")) %>%
  filter(week_num != "wk6_7")
metrics <- c("stimulus_expenses", "spend_credit", "spend_ui", 
             "spend_stimulus", "spend_savings")

wks <- c("wk1_2", "wk2_3", "wk3_4", "wk4_5", "wk5_6", "wk6_7")
race_indicators <- unique(spend_vars$race_var)
geographies <- unique(spend_vars$geography)

# create combo for weeks without
full_combo <- expand_grid(
  geography = geographies,
  metric = metrics,
  race_var = race_indicators,
  week_num = wks
) %>%
  mutate(mean = NA_real_,
         se = NA_real_,
         geo_type = case_when(
           geography == "US" ~ "national",
           nchar(as.character(geography)) == 2 ~ "state",
           T ~ "msa"
         ),
         moe_95 = NA_real_,
         moe_95_lb = NA_real_,
         moe_95_ub = NA_real_,
         sigdiff = NA_real_)


combined_data <- rbind(all_data, spend_vars, full_combo)

week_crosswalk <- tibble::tribble(
  ~week_num, ~date_int,
  "wk1_2", paste("Apr. 23\u2013", "May 12", sep = ""),
  "wk2_3", paste("May 7\u2013", "19", sep = ""),
  "wk3_4", paste("May 14\u2013", "26", sep = ""),
  "wk4_5", paste("May 21\u2013", "June 2", sep = ""),
  "wk5_6", paste("May 28\u2013", "June 9", sep = ""),
  "wk6_7", paste("June 4\u2013", "16", sep = ""),
  "wk7_8", paste("June 11\u2013", "23", sep = ""),
  "wk8_9", paste("June 18\u2013", "30", sep = ""),
  "wk9_10", paste("June 25\u2013", "July 7", sep = "")
)

data_out <- left_join(combined_data, week_crosswalk, by = "week_num")
write.csv(data_out, here("data/final-data", "rolling_all_to_current_week_new_vars.csv"))