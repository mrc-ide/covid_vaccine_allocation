library(dplyr)
library(readr)
library(tidyr)
library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)

source("R/functions_optim.R")

#######################################################################
# User-defined inputs

sr_list <- c("reduce_efficacy", "immunosenescence", "mode_disease", "lower_Rt2", "hs_constraints_absent", "reduce_inf")

for (sr in sr_list){

# set vaccine dose constraint
budget = 2e9 * 0.85 / 2

#######################################################################
# read in dataframe
d_main <- read_csv(paste0("optim_dataframes/optim_dataframe_income_fine_sensitivity_", sr, ".csv"))

#######################################################################

# Generate all possibilities for each income group partitioned into country sizes
partition_size <- read_csv("data/country_pop_income.csv") %>%
  arrange(-partition_size) %>%
  mutate(cs = cumsum(partition_size),
         prop = cs/max(cs)) %>%
  filter(cs<=0.95*max(cs)) %>%
  select(-cs, -prop)

income_group <- c("HIC", "UMIC", "LMIC", "LIC")
age_target <- unique(d_sub$age_target)

p <- expand_grid(income_group, age_target) %>%
  left_join(partition_size) %>%
  mutate(income_group_p = paste0(income_group, "_", country_code))

# Join with results of simulations and calculate the deaths averted and doses for each partition size
d <- left_join(p, d_main) %>%
  group_by(income_group, age_target, coverage) %>%
  mutate(income_group_size = sum(partition_size)) %>%
  ungroup() %>%
  mutate(deaths_averted_2021 = round(deaths_averted_2021 * partition_size / pop_2019),
         vaccine_n_2021 = round(vaccine_n_2021 * partition_size / pop_2019))

# Format raw data
input <- d %>%
  # Remove missing values
  filter(!is.na(deaths_averted_2021)) %>%
  # Store the country code as a separate variable
  mutate(country = income_group_p) %>%
  # Have deaths_averted and doses columns the algorithm will recognise
  rename(deaths_averted = deaths_averted_2021, doses = vaccine_n_2021)

#######################################################################
# Run optimisation
optimal <- run_optim(input, budget)

#######################################################################
# Save results
write_csv(optimal, paste0("between_country_optim_outputs/optimal_", sr, ".csv"))
}