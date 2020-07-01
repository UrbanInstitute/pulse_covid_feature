library(tidyverse)
library(here)
library(srvyr)
library(survey)
library(fastDummies)
devtools::install_github("hadley/multidplyr")
library(multidplyr)
library(parallel)

# Notes for Alena: I've written a set of functions that will calculate the se of
# the differences bw subgroup and cbsa average (or cbsa avg and national avg)
# for all cbsa-metric-race-week combinations (4050 unique combinations).
# This is done in the get_se_diff_cbsa() and get_se_group_vs_avg_cbsa()
# functions below. This set of functions needs to be replicated for states.
# Note that running the function takes up a LOT of ram and probably should
# be done on a beefy instance.My computer kept giving me not enough RAM
# errors. Below are short descriptions of the two important functions I've created.

# get_se_group_vs_avg_cbsa(): This construct all unique combinations of
# metrics, race, weeks, and cbsa as a dataframe, then maps over every row
# of the dataframe and applies the get_se_diff_cbsa() function. This generates
# the mean_difference and standard error bw subgroups and cbsa avg (or cbsa
# and national avg) and appends it to the all unique combinations dataframe.

# get_se_diff_cbsa(): For a given metric-race-week-cbsa combination, uses the
# svyby and svycontrast functiions to calcaulte the correct replicate weight
# adjusted means and standard errors for the difference bw the subgroup and the
# state avg (or state avg and national avg if race_var == total).


##  Read in and clean data


puf_all_weeks <- read_csv(here("data/intermediate-data", "pulse_puf_all_weeks.csv"))

# Set parameters
# May want to pull this from a set variable in main update script
CUR_WEEK <- puf_all_weeks %>%
  pull(week_x) %>%
  max()


puf_all_weeks <- puf_all_weeks %>%
  # For the uninsured variable, we filter out people over 65 from the denominator
  mutate(
    insured_public = case_when(
      tbirth_year < 1956 ~ NA_real_,
      TRUE ~ insured_public
    ),
    uninsured = case_when(
      tbirth_year < 1956 ~ NA_real_,
      TRUE ~ uninsured
    )
  ) %>%
  # Create one hot encoding of cbsas and states for easy use with survey pkg
  fastDummies::dummy_cols("cbsa_title") %>%
  fastDummies::dummy_cols("state") %>%
  janitor::clean_names() %>%
  # Add race indicator variables for easy use with survey package
  mutate(
    black = case_when(
      str_detect(hisp_rrace, "Black alone") ~ 1,
      TRUE ~ 0
    ),
    white = case_when(
      str_detect(hisp_rrace, "White") ~ 1,
      TRUE ~ 0
    ),
    hispanic = case_when(
      str_detect(hisp_rrace, "Latino") ~ 1,
      TRUE ~ 0
    ),
    asian = case_when(
      str_detect(hisp_rrace, "Asian") ~ 1,
      TRUE ~ 0
    ),
    other = case_when(
      str_detect(hisp_rrace, "Two or") ~ 1,
      TRUE ~ 0
    )
  )


svy <- puf_all_weeks %>%
  as_survey_rep(
    repweights = dplyr::matches("pweight[0-9]+"),
    weights = pweight,
    type = "BRR",
    mse = TRUE
  )

# Construct crosswalk reference dataframe

# Helper function to dynamically generate crosswalk reference dataframe
construct_overlap_intervals_df <- function(x) {
  # INPUT:
  #   x: integer

  ### Construct week number vector
  week_vec <- 1:x
  # Get middle weeks and repeat each twice
  middle_nums <- week_vec[c(-1, -length(week_vec))]
  middle_nums <- rep(middle_nums, each = 2)
  # Construct week_num vector where middle numbers are repeated twice
  wk_nums <- c(week_vec[1], middle_nums, tail(week_vec, n = 1))
  wk_nums <- paste0("wk", wk_nums)



  ### Construct week_Average vector
  interval_vec <- c()
  while (x != 1) {
    previous_num <- x - 1
    interval <- paste0("wk", previous_num, "_", x)
    interval_vec <- c(interval_vec, interval)

    x <- x - 1
  }
  # sort alphabetically and repeat each twice
  wk_int <- sort(interval_vec) %>% rep(each = 2)

  ### Create dataframe
  result <- tibble(
    week_num = wk_nums,
    week_int = wk_int
  )



  return(result)
}


week_crosswalk <- construct_overlap_intervals_df(CUR_WEEK)


puf_all_weeks_rolling <- puf_all_weeks %>%
  mutate_at(vars(starts_with("pweight")), funs(. / 2)) %>% # divide pweight and repweights by 2
  right_join(week_crosswalk, by = "week_num") %>% # create week range column
  rename("week_pt" = "week_num", "week_num" = "week_int")


svy_rolling <- puf_all_weeks_rolling %>%
  as_survey_rep(
    repweights = dplyr::matches("pweight[0-9]+"),
    weights = pweight,
    type = "BRR",
    mse = TRUE
  )






# Tested function to calculate all means/SEs and diff mean/SEs for
# all geography/race combinations (except US, which is handled separately)
get_se_diff <- function(..., svy = svy_rolling) {
  # INPUT:
  #    ...: Must be a dataframe with the following columns:
  #     metric, race_indicator(dummy race var), geography, week. Idea is to
  #     use expand_grid() with combinations of all the above so we can
  #     can calculate se of difference for all combinations in one go using
  #     this function.
  dots <- list(...)

  metric_formula <- as.formula(paste0("~", dots$metric))
  race_formula <- as.formula(paste0("~", dots$race_indicator))

  if (dots$race_indicator == "total") {
    geo_formula <- as.formula(paste0("~", dots$geo_col))
    x <- svyby(metric_formula, geo_formula, svy %>%
      filter(week_num == dots$week),
    svymean,
    na.rm = T,
    return.replicates = T,
    covmat = T
    )

    mean <- x %>%
      filter(!!sym(dots$geo_col) == 1) %>%
      pull(!!sym(dots$metric))
    se <- x %>%
      filter(!!sym(dots$geo_col) == 1) %>%
      pull(se) * 2
    other_mean <- x %>%
      filter(!!sym(dots$geo_col) == 0) %>%
      pull(!!sym(dots$metric))
    other_se <- x %>%
      filter(!!sym(dots$geo_col) == 0) %>%
      pull(se)
  }
  # if race_indicator isn't total, then compare subgroup to cbsa avg
  else {

    # identify whether geography is state or cbsa
    geo_col_name <- ifelse(grepl("cbsa", dots$geo_col, fixed = TRUE), "cbsa_title", "state")


    # handle exception in which all NAs for a given race/geo/week/metric
    t <- svy %>%
      filter(!!sym(geo_col_name) == dots$geography) %>%
      filter(week_num == dots$week) %>%
      filter(!!sym(dots$race_indicator) == 1) %>%
      pull(!!sym(dots$metric))

    if (all(is.na(t))) {
      print("error!")
      return(tibble(
        mean = NA,
        se = 0,
        other_mean = NA,
        other_se = 0,
        diff_mean = 0,
        diff_se = 0
      ))
    } else {
      # Use svyby to compute mean (and replicate means) for race and non race var population
      # (ie black and nonblack population)
      x <- svyby(metric_formula, race_formula, svy %>%
        filter(!!sym(geo_col_name) == dots$geography) %>%
        filter(week_num == dots$week),
      svymean,
      na.rm = T,
      return.replicates = T,
      covmat = T
      )

      mean <- x %>%
        filter(!!sym(dots$race_indicator) == 1) %>%
        pull(!!sym(dots$metric))
      se <- x %>%
        filter(!!sym(dots$race_indicator) == 1) %>%
        pull(se) * 2
      other_mean <- x %>%
        filter(!!sym(dots$race_indicator) == 0) %>%
        pull(!!sym(dots$metric))
      other_se <- x %>%
        filter(!!sym(dots$race_indicator) == 0) %>%
        pull(se)
    }
  }

  # Use svycontrast to calulate se bw race and nonrace (ie black and non black) population
  contrast <- svycontrast(x, contrasts = list(diff = c(1, -1)))
  diff_mean <- contrast %>% as.numeric()
  diff_se <- contrast %>%
    attr("var") %>%
    sqrt() %>%
    {
      . * 2
    }

  return(tibble(
    mean = mean,
    se = se,
    other_mean = other_mean,
    other_se = other_se,
    diff_mean = diff_mean,
    diff_se = diff_se
  ))
}



get_se_group_vs_avg_parallel <- function(metrics, race_indicators, svy = svy_rolling) {
  cbsa_names <- svy %>%
    pull(cbsa_title) %>%
    unique() %>%
    na.omit()
  state <- svy %>%
    pull(state) %>%
    unique() %>%
    na.omit()
  geography <- c(cbsa_names, state)
  # mirror clean column names created by fastDummies
  geo_cols <- c(
    paste0("cbsa_title_", janitor::make_clean_names(cbsa_names)),
    paste0("state_", janitor::make_clean_names(state))
  )
  # crosswalk between geography names and geography dummy column names
  geo_xwalk <- tibble(geography = geography, geo_col = geo_cols)
  wks <- svy %>%
    pull(week_num) %>%
    unique() %>%
    na.omit()

  full_combo <- expand_grid(
    metric = metrics,
    race_indicator = race_indicators,
    geo_col = geo_cols,
    week = wks
  ) %>%
    left_join(geo_xwalk, by = "geo_col")

  # for testing (as running on all 4080 combintaions takes up too much RAM)
  full_combo <- full_combo %>% filter(race_indicator %in% c("black", "total"), metric == "mortgage_not_conf")

  # group <- rep(1:cl, length.out = nrow(full_combo))
  # full_combo <- bind_cols(tibble(group), full_combo)

  # set up cluster
  cl <- detectCores()
  cluster <- new_cluster(cl - 1)
  full_partition <- full_combo %>%
    group_by(geography) %>%
    partition(cluster)

  # get mean and se for diff bw subgroup and (total population -subgroup)
  se_info <- full_partition %>%
    pmap_df(get_se_diff) %>%
    collect()
  full_combo_appended <- full_combo %>%
    bind_cols(se_info) %>%
    mutate(geo_type = ifelse(geography %in% cbsa_names, "msa", "state")) %>%
    select(-geo_col)

  return(full_combo_appended)
}


metrics <- c(
  "uninsured",
  "insured_public",
  "inc_loss",
  "expect_inc_loss",
  "rent_not_conf",
  "mortgage_not_conf",
  "food_insufficient",
  "classes_cancelled",
  "depression_anxiety_signs"
)
race_indicators <- c("black", "asian", "hispanic", "white", "other", "total")

# Note for Alena: The below code works, but chews up a lot of RAM and often errors out with
# errors that RAM isn't enough. I suggest running on instance with beefier RAM/CPU
# Update: ran on c5.4xlarge instance and took 2.5 hours (with print statements for debugging)
start <- Sys.time()
all_diff_ses <- get_se_group_vs_avg_parallel(metrics = metrics, race_indicators = race_indicators)
end <- Sys.time()

print(end - start)
print(nrow(all_diff_ses))

write.csv(all_diff_ses, here("data/intermediate-data", "all_diff_ses.csv"))

# functions to calculate US total means/SEs
calculate_se_us <- function(metric, svy) {
  se_df <- svy %>%
    filter(!is.na(!!sym(metric))) %>%
    group_by(week_num, hisp_rrace) %>%
    summarise(mean = survey_mean(!!sym(metric), na.rm = TRUE)) %>%
    # pull(out) %>%
    mutate(
      se = mean_se * 2,
      metric = metric,
      geography = "US"
    ) %>%
    select(week_num, geography, hisp_rrace, mean, se, metric)

  return(se_df)
}

calculate_se_us_total <- function(metric, svy) {
  se_df <- svy %>%
    filter(!is.na(!!sym(metric))) %>%
    group_by(week_num) %>%
    summarise(mean = survey_mean(!!sym(metric), na.rm = TRUE)) %>%
    # pull(out) %>%
    mutate(
      se = mean_se * 2,
      metric = metric,
      geography = "US",
      race_var = "total"
    ) %>%
    select(week_num, geography, race_var, mean, se, metric)

  return(se_df)
}

format_feature <- function(data, race_xwalk, geo) {
  data <- data %>%
    mutate(
      moe_95 = se * 1.96,
      moe_95_lb = mean - moe_95,
      moe_95_ub = mean + moe_95,
      geo_type = geo
    ) %>%
    left_join(race_xwalk, by = "hisp_rrace") %>%
    select(geography, metric, week_num, race_var, mean, se, moe_95, moe_95_lb, moe_95_ub, geo_type)

  return(data)
}

format_feature_total <- function(data, geo) {
  data <- data %>%
    mutate(
      moe_95 = se * 1.96,
      moe_95_lb = mean - moe_95,
      moe_95_ub = mean + moe_95,
      geo_type = geo
    ) %>%
    select(geography, metric, week_num, race_var, mean, se, moe_95, moe_95_lb, moe_95_ub, geo_type)

  return(data)
}

race_xwalk <- data.frame(
  "race_var" = c("white", "black", "asian", "hispanic", "other"),
  "hisp_rrace" = c(
    "White alone, not Hispanic",
    "Black alone, not Hispanic",
    "Asian alone, not Hispanic",
    "Hispanic or Latino (may be of any race)",
    "Two or more races + Other races, not Hispanic"
  )
)


# calculate US-wide means for each metric/week
us_rolling <- map_df(metrics, calculate_se_us, svy = svy_rolling)
write.csv(us_rolling, here("data/intermediate-data", str_glue("us_rolling_se_to_week{CUR_WEEK}.csv")))
us_rolling_total <- map_df(metrics, calculate_se_us_total, svy = svy_rolling)
write.csv(us_rolling_total, here("data/intermediate-data", str_glue("us_rolling_total_se_to_week{CUR_WEEK}.csv")))

us_rolling_out <- format_feature(us_rolling, race_xwalk, "national")
us_total_rolling_out <- format_feature_total(us_rolling_total, "national")

all_diff_ses_out <- all_diff_ses %>%
  mutate(
    moe_95 = se * 1.96,
    moe_95_lb = mean - moe_95,
    moe_95_ub = mean + moe_95,
    sigdiff = ifelse(abs(diff_mean / diff_se) > 1.96, 1, 0)
  ) %>%
  rename(
    race_var = race_indicator,
    week_num = week
  ) %>%
  select(-other_mean, -other_se, -diff_mean, -diff_se)

rolling_all <- bind_rows(all_diff_ses_out, us_rolling_out, us_total_rolling_out)
write_csv(rolling_all, here("data/final-data", "rolling_all_to_current_week_sd_update.csv"))



##### SCRATCH WORK #######

# test that all means/SEs the same as srvyr method
rolling_test <- read_csv(here("data/final-data", "rolling_all_to_current_week_sd.csv")) %>%
  select(-sigdiff) %>%
  arrange(geography, metric, week_num, race_var)

rolling_all_test <- rolling_all %>%
  filter(!is.na(mean)) %>%
  select(geography, metric, week_num, race_var, mean, se, moe_95, moe_95_lb, moe_95_ub, geo_type) %>%
  arrange(geography, metric, week_num, race_var)

all.equal(rolling_test, rolling_all_test) # returns TRUE

rolling_test_sd <- read_csv(here("data/final-data", "rolling_all_to_current_week_sd.csv"))
sum(rolling_test_sd$sigdiff) # 5410 difference significant in old measure
sum(rolling_all$sigdiff, na.rm = TRUE) # 6734 difference significant in new measure


svy_rolling <- puf_all_weeks_rolling %>%
  as_survey_rep(
    repweights = dplyr::matches("pweight[0-9]+"),
    weights = pweight,
    type = "BRR",
    mse = FALSE
  )


# example of using regression to get correct SE for differences
svyglm(inc_loss ~ black,
  svy_rolling %>%
    filter(week_num == "wk1_2") %>%
    filter(cbsa_title == "Atlanta-Sandy Springs-Roswell, GA"),
  na.action = "na.omit"
) %>%
  summary()





## Calculating Standard errors by hand using Census formulas and manual filtering
# of dataframes

compute_replicate_diff <- function(mean_obj1, mean_obj2) {
  replicates_1 <- mean_obj1$replicates
  replicates_2 <- mean_obj2$replicates

  theta_i <- replicates_1 - replicates_2
  theta_hat <- mean_obj1$mean %>%
    as.numeric() - mean_obj2$mean %>%
    as.numeric()
  var_theta <- 4 / 80 * sum((theta_i - theta_hat)^2)
  se_diff <- sqrt(var_theta)
  return(c(mean = theta_hat, se = se_diff))
}

# 1: For Atlanta income loss + black race variable cross checked these numbers
# with Alena's calculations and calculations from above

tot <- svymean(~inc_loss, svy_rolling %>%
  filter(week_num == "wk1_2") %>%
  filter(cbsa_title == "Atlanta-Sandy Springs-Roswell, GA"),
na.rm = T, return.replicates = TRUE
)
replicates_total <- tot$replicates

nb <- svymean(~inc_loss, svy_rolling %>%
  filter(week_num == "wk1_2") %>%
  filter(!str_detect(hisp_rrace, "Black")) %>%
  filter(cbsa_title == "Atlanta-Sandy Springs-Roswell, GA"),
na.rm = T, return.replicates = TRUE
)
replicates_not_black <- nb$replicates

b <- svymean(~inc_loss, svy_rolling %>%
  filter(week_num == "wk1_2") %>%
  filter(str_detect(hisp_rrace, "Black")) %>%
  filter(cbsa_title == "Atlanta-Sandy Springs-Roswell, GA"),
na.rm = T, return.replicates = TRUE
)
replicate_black <- b$replicates


params_black_nonblack <- compute_replicate_diff(b, nb)
mean_black_nonblack <- params_black_nonblack[["mean"]]
se_black_nonblack <- params_black_nonblack[["se"]]
tstat_black_nonblack <- mean_black_nonblack / se_black_nonblack

params_black_total <- compute_replicate_diff(tot, b)
mean_black_total <- params_black_total[["mean"]]
se_black_total <- params_black_total[["se"]]
tstat_black_total <- mean_black_total / se_black_total

# 2: For CA rent_not_conf wk4_5 asian

tot <- svymean(~rent_not_conf, svy_rolling %>%
  filter(week_num == "wk4_5") %>%
  filter(state == "CA"),
na.rm = T, return.replicates = TRUE
)
replicates_total <- tot$replicates

na <- svymean(~rent_not_conf, svy_rolling %>%
  filter(week_num == "wk4_5") %>%
  filter(!str_detect(hisp_rrace, "Asian")) %>%
  filter(state == "CA"),
na.rm = T, return.replicates = TRUE
)
replicates_not_black <- nb$replicates

a <- svymean(~rent_not_conf, svy_rolling %>%
  filter(week_num == "wk4_5") %>%
  filter(str_detect(hisp_rrace, "Asian")) %>%
  filter(state == "CA"),
na.rm = T, return.replicates = TRUE
)
replicate_black <- b$replicates


params_black_nonblack <- compute_replicate_diff(a, na)
mean_black_nonblack <- params_black_nonblack[["mean"]]
se_black_nonblack <- params_black_nonblack[["se"]]
tstat_black_nonblack <- mean_black_nonblack / se_black_nonblack
print(tstat_black_nonblack)

ra_stats <- all_diff_ses %>%
  filter(
    week == "wk4_5", race_indicator == "asian",
    metric == "rent_not_conf", geography == "CA"
  ) %>%
  select(diff_mean, diff_se)

abs(ra_stats$diff_mean) == abs(mean_black_nonblack)
ra_se <- ra_stats$diff_se %>% as.numeric()
round(ra_se, 8) == round(se_black_nonblack, 8)

params_black_total <- compute_replicate_diff(tot, a)
mean_black_total <- params_black_total[["mean"]]
se_black_total <- params_black_total[["se"]]
tstat_black_total <- mean_black_total / se_black_total
print(tstat_black_total)

# 3: For WY classes_cancelled wk4_5 other

tot <- svymean(~classes_cancelled, svy_rolling %>%
  filter(week_num == "wk4_5") %>%
  filter(state == "WY"),
na.rm = T, return.replicates = TRUE
)
replicates_total <- tot$replicates

na <- svymean(~classes_cancelled, svy_rolling %>%
  filter(week_num == "wk4_5") %>%
  filter(!str_detect(hisp_rrace, "Other")) %>%
  filter(state == "WY"),
na.rm = T, return.replicates = TRUE
)
replicates_not_black <- nb$replicates

a <- svymean(~classes_cancelled, svy_rolling %>%
  filter(week_num == "wk4_5") %>%
  filter(str_detect(hisp_rrace, "Other")) %>%
  filter(state == "WY"),
na.rm = T, return.replicates = TRUE
)
replicate_black <- b$replicates


params_black_nonblack <- compute_replicate_diff(a, na)
mean_black_nonblack <- params_black_nonblack[["mean"]]
se_black_nonblack <- params_black_nonblack[["se"]]
tstat_black_nonblack <- mean_black_nonblack / se_black_nonblack
print(tstat_black_nonblack)

ra_stats <- all_diff_ses %>%
  filter(
    week == "wk4_5", race_indicator == "other",
    metric == "classes_cancelled", geography == "WY"
  ) %>%
  select(diff_mean, diff_se)

abs(ra_stats$diff_mean) == abs(mean_black_nonblack)
ra_se <- ra_stats$diff_se %>% as.numeric()
round(ra_se, 8) == round(se_black_nonblack, 8)

params_black_total <- compute_replicate_diff(tot, a)
mean_black_total <- params_black_total[["mean"]]
se_black_total <- params_black_total[["se"]]
tstat_black_total <- mean_black_total / se_black_total
print(tstat_black_total)



########################

#####################################
#### Old CBSA-Specific Functions ####
#####################################

### Define specific se diff functions for calculating standard errors for
# difference between state/metro/nat avg vs subgroup avg

get_se_diff_cbsa <- function(..., svy = svy_rolling) {
  # Helper function to generate mean and se for difference bw cbsa avg
  # and subgroup avg given metric, week,
  # cbsa_name and race_indicator column in a tibble. This fxn is fed into
  # pmap so we make use of ... so we can use any column in the inputted
  # dataframe with dots$colname
  # INPUT:
  #    ...: Must be a dataframe with the following columns:
  #     metric, race_indicator(dummy race var), cbsa_name, and week. Idea is to
  #     use expand_grid() with combinations of all the above so we can
  #     can calculate se of difference for all combinations in one go using
  #     this function.
  dots <- list(...)

  # metric = "inc_loss"
  # race_indicator = "black"
  # cbsa_name = "Atlanta-Sandy Springs-Roswell, GA"
  # week = "wk1_2"
  # race_indicator ="black"



  metric_formula <- as.formula(paste0("~", dots$metric))
  race_formula <- as.formula(paste0("~", dots$race_indicator))

  print(dots$metric)
  print(dots$race_indicator)
  print(dots$week)
  print(dots$cbsa_name)

  # if race_indicator is total, need to compare cbsa vs national avg
  if (dots$race_indicator == "total") {
    # This column defmirrors the def used to create the dummy cbsa columns
    cbsa_name <- paste0("cbsa_title_", janitor::make_clean_names(dots$cbsa_name))
    cbsa_formula <- as.formula(paste0("~", cbsa_name))

    x <- svyby(metric_formula, cbsa_formula, svy %>%
      filter(week_num == dots$week),
    svymean,
    na.rm = T,
    return.replicates = T,
    covmat = T
    )

    mean <- x %>%
      filter(!!sym(cbsa_name) == 1) %>%
      pull(!!sym(dots$metric))
    se <- x %>%
      filter(!!sym(cbsa_name) == 1) %>%
      pull(se) * 2
    other_mean <- x %>%
      filter(!!sym(cbsa_name) == 0) %>%
      pull(!!sym(dots$metric))
    other_se <- x %>%
      filter(!!sym(cbsa_name) == 0) %>%
      pull(se)
  }
  # if race_indicator isn't total, then compare subgroup to cbsa avg
  else {

    # Use svyby to compute mean (and replicate means) for race and non race var population
    # (ie black and nonblack population)
    x <- svyby(metric_formula, race_formula, svy %>%
      filter(cbsa_title == dots$cbsa_name) %>%
      filter(week_num == dots$week),
    svymean,
    na.rm = T,
    return.replicates = T,
    covmat = T
    )

    mean <- x %>%
      filter(!!sym(dots$race_indicator) == 1) %>%
      pull(!!sym(dots$metric))
    se <- x %>%
      filter(!!sym(dots$race_indicator) == 1) %>%
      pull(se) * 2
    other_mean <- x %>%
      filter(!!sym(dots$race_indicator) == 0) %>%
      pull(!!sym(dots$metric))
    other_se <- x %>%
      filter(!!sym(dots$race_indicator) == 0) %>%
      pull(se)
  }



  # Use svycontrast to calulate se bw race and nonrace (ie black and non black) population
  contrast <- svycontrast(x, contrasts = list(diff = c(1, -1)))
  diff_mean <- contrast %>% as.numeric()
  diff_se <- contrast %>%
    attr("var") %>%
    sqrt() %>%
    {
      . * 2
    }
  return(tibble(
    mean = mean,
    se = se,
    other_mean = other_mean,
    other_se = other_se,
    diff_mean = diff_mean,
    diff_se = diff_se
  ))
}


get_se_group_vs_avg_cbsa <- function(metrics, race_indicators, svy = svy_rolling) {
  cbsa_names <- svy %>%
    pull(cbsa_title) %>%
    unique() %>%
    na.omit()
  state <- svy %>%
    pull(state) %>%
    unique() %>%
    na.omit()
  wks <- svy %>%
    pull(week_num) %>%
    unique() %>%
    na.omit()

  full_combo_cbsa <- expand_grid(
    metric = metrics,
    race_indicator = race_indicators,
    cbsa_name = cbsa_names,
    week = wks
  )

  # for testing (as running on all 4080 combintaions takes up too much RAM)
  full_combo_cbsa <- full_combo_cbsa %>% filter(race_indicator %in% c("black"), metric == "mortgage_not_conf")


  # get mean and se for diff bw subgroup and (total population -subgroup)
  se_info <- full_combo_cbsa %>% pmap_df(get_se_diff_cbsa)
  full_combo_cbsa_appended <- full_combo_cbsa %>% bind_cols(se_info)

  return(full_combo_cbsa_appended)
}
