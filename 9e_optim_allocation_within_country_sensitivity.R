### Supplementary Figures for Within-Country Vaccine Allocation #################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(stringr)
library(data.table)
library(patchwork)
library(ggplot2)
library(coop)

################################################################################

sr_list <- c("reduce_efficacy", "immunosenescence", "mode_disease", "lower_Rt2", "hs_constraints_absent", "reduce_inf")

for (sr in sr_list){

### Load outputs ###############################################################
d_out_all <- read_csv(paste0("optim_dataframes/optim_dataframe_income_fine_sensitivity_", sr, ".csv")) %>%
  mutate(pop_age_target = round(pop_2019 * proportion / 100),
         vaccine_n_2021 = round(coverage*pop_age_target),
         income_group = factor(income_group, levels = c("HIC","UMIC", "LMIC", "LIC")),
         relative_constraint = vaccine_n_2021 / pop_2019 * 100) %>%
  filter(sensitivity_run == sr) %>%
  # Create columns for the front function to use
  mutate(cost = vaccine_n_2021,
         y = round(deaths_averted_2021))

#### Optimal solution frontier #################################################
# Subset for non-dominated solution = the frontier - this is done for each income group
# Keep frontier up to the maximum deaths averted; beyond that level of supply keep everything
sub_HIC <- filter(d_out_all, income_group == "HIC")
threshold_HIC <- sub_HIC[which(sub_HIC$deaths_averted_2021 == max(sub_HIC$deaths_averted_2021)),]$relative_constraint

d_out_HIC <- rbind(
  front(sub_HIC),
  filter(sub_HIC, relative_constraint >= threshold_HIC, deaths_averted_2021 >= 0.9999 * max(sub_HIC$deaths_averted_2021)))

sub_UMIC <- filter(d_out_all, income_group == "UMIC")
threshold_UMIC <- sub_UMIC[which(sub_UMIC$deaths_averted_2021 == max(sub_UMIC$deaths_averted_2021)),]$relative_constraint

d_out_UMIC <- rbind(
  front(sub_UMIC),
  filter(sub_UMIC, relative_constraint >= threshold_UMIC, deaths_averted_2021 >= 0.9999 * max(sub_UMIC$deaths_averted_2021)))

sub_LMIC <- filter(d_out_all, income_group == "LMIC")
threshold_LMIC <- sub_LMIC[which(sub_LMIC$deaths_averted_2021 == max(sub_LMIC$deaths_averted_2021)),]$relative_constraint

d_out_LMIC <- rbind(
  front(sub_LMIC),
  filter(sub_LMIC, relative_constraint >= threshold_LMIC, deaths_averted_2021 >= 0.9999 * max(sub_LMIC$deaths_averted_2021)))

sub_LIC <- filter(d_out_all, income_group == "LIC")
threshold_LIC <- sub_LIC[which(sub_LIC$deaths_averted_2021 == max(sub_LIC$deaths_averted_2021)),]$relative_constraint

d_out_LIC <- rbind(
  front(sub_LIC),
  filter(sub_LIC, relative_constraint >= threshold_LIC, deaths_averted_2021 >= 0.9999 * max(sub_LIC$deaths_averted_2021)))

d_out <- rbind(d_out_HIC, d_out_UMIC, d_out_LMIC, d_out_LIC)
#### Non-optimal within-country strategies #####################################

# target older first
age_target <- read_csv("data/age_target_strategy_old.csv")$age_target
income_group <- unique(d$income_group)
params <- crossing(income_group, age_target)
out_older_first <- left_join(params, d_out_all) %>%
  mutate(relative_constraint = vaccine_n_2021 / pop_2019 * 100) %>%
  mutate(strategy = "Target older groups first")

# non-optim within-country strategy: target working age first
age_target <- read_csv("data/age_target_strategy_working.csv")$age_target
income_group <- unique(d$income_group)
params <- crossing(income_group, age_target)
out_working_first <- left_join(params, d_out_all) %>%
  mutate(relative_constraint = vaccine_n_2021 / pop_2019 * 100) %>%
  mutate(strategy = "Target working-age first")

# optimal frontier
out_optim <- d_out %>%
  mutate(strategy = "Optimal")

# combine all
df1 <- rbind(out_older_first, out_working_first) %>%
  rbind(out_optim)

write_csv(df1, paste0("optim_dataframes/fig_4_inputs_", sr, ".csv"))

}