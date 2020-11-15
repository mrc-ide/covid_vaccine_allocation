### Create dataframe for the allocation optimisation ###########################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(stringr)
################################################################################

sr_list <- c("reduce_efficacy", "immunosenescence", "mode_disease", "lower_Rt2", "hs_constraints_absent", "reduce_inf")

for (sr in sr_list){

### Load outputs ###############################################################
raw <- read_csv(paste0("cluster_outputs_sensitivity/AGGREGATOR_", sr, ".csv"))
scenarios <- read_csv("cluster_outputs_sensitivity/scenarios.csv") %>%
  filter(sensitivity_run == sr)
d <- left_join(raw, scenarios) %>%
  #rename("deaths" = "d") %>%
  select(-run_number, -life_years_lost)

d_cf <- filter(d, age_target == "0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0") %>%
  select(-coverage, -age_target, -vaccine_n) %>%
  rename("deaths_cf" = "deaths")

# join with cf scenario and calculate deaths averted
d <- d %>%
  left_join(d_cf) %>%
  mutate(deaths_averted_2021 = deaths_cf - deaths)

################################################################################

age_target <- unique(d$age_target)

#################################################################################
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
  )

write_csv(d_main_income_region, paste0("optim_dataframes/optim_dataframe_income_fine_sensitivity_", sr, ".csv"))

}
