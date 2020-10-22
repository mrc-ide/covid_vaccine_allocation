### Create dataframe for the allocation optimisation ###########################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(stringr)
###############################################################################

### Load outputs ###############################################################
raw <- read_csv("cluster_outputs_default/AGGREGATOR.csv")
scenarios <- read_csv("cluster_outputs_default/scenarios.csv")
d <- left_join(raw, scenarios) %>%
  select(-run_number, -directory_out)

d_cf <- filter(d, age_target == "0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0") %>%
  select(-coverage, -age_target) %>%
  rename("deaths_cf" = "deaths",
         "life_years_lost_cf" = "life_years_lost")

# join with cf scenario and calculate deaths averted
d <- d %>%
  left_join(d_cf) %>%
  mutate(deaths_averted_2021 = deaths_cf - deaths,
         additional_life_years_2021 = life_years_lost_cf - life_years_lost)

################################################################################
age_target <- unique(d$age_target)

################################################################################

# create dataframe by income group, population, and proportion for that age target
pop_dat <- read_csv("data/income_region_pop_proportions_fine.csv")

y <- expand_grid(income_group = pop_dat$income_group,
            age_target = age_target) %>%
  left_join(pop_dat) %>%
  mutate(proportion = 0)

for (i in 1:nrow(y)){
  proportion <-  as.numeric(unlist(str_split(y[i,2], "_")))
  y[i,]$proportion <- sum(y[i,4:20]*proportion)
}

d_main_income_region <- y %>%
  select(income_group, age_target, pop_2019, proportion) %>%
  left_join(d, by = c("income_group", "age_target")) %>%
  mutate(deaths_averted_2021 = deaths_averted_2021 / 50e6 * pop_2019,
         vaccine_n_2021 = pop_2019 * proportion/100 * coverage,
         dapd_2021 = deaths_averted_2021 / vaccine_n_2021,
         additional_life_years_2021 = additional_life_years_2021 / 50e6 * pop_2019
  )

write_csv(d_main_income_region, "optim_dataframes/optim_dataframe_income_fine.csv")

################################################################################
# read in the immunosenescence runs
raw_imm <- read_csv("cluster_outputs_default_immunosenescence/AGGREGATOR.csv")
scenarios_imm <- read_csv("cluster_outputs_default_immunosenescence/scenarios.csv")
d_imm <- left_join(raw_imm, scenarios_imm) %>%
  select(-run_number, -directory_out)

d_cf_imm <- filter(d_imm, age_target == "0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0") %>%
  select(-coverage, -age_target) %>%
  rename("deaths_cf" = "deaths",
         "life_years_lost_cf" = "life_years_lost")

# join with cf scenario and calculate deaths averted
d_imm <- d_imm %>%
  left_join(d_cf_imm) %>%
  mutate(deaths_averted_2021 = deaths_cf - deaths,
         additional_life_years_2021 = life_years_lost_cf - life_years_lost)

d_main_income_region_imm <- y %>%
  select(income_group, age_target, pop_2019, proportion) %>%
  left_join(d_imm, by = c("income_group", "age_target")) %>%
  mutate(deaths_averted_2021 = deaths_averted_2021 / 50e6 * pop_2019,
         vaccine_n_2021 = pop_2019 * proportion/100 * coverage,
         dapd_2021 = deaths_averted_2021 / vaccine_n_2021,
         additional_life_years_2021 = additional_life_years_2021 / 50e6 * pop_2019
  )

write_csv(d_main_income_region_imm, "optim_dataframes/optim_dataframe_income_fine_immunosenescence.csv")
