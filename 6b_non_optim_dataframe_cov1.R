### Create dataframe for the fixed allocation ###########################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

### Load outputs ###############################################################
d <- readRDS("output/6_non_optim_cov1.rds") %>%
  select(t_start, R0, reduction1, reduction2, immunosenescence, mode, hs_constraints, efficacy, duration_R, duration_V, vaccine_start, vaccine_period, dur_vacc_delay, reduce_inf, vaccine_start, seeding_cases, timing1, timing2, strategy, income_group, target_group, age_target, coverage_children, coverage_middle, coverage_old, deaths_averted_2021, vaccine_n_2021) %>%
  filter(!(coverage_children == 0 & coverage_middle == 0 & coverage_old == 0))

################################################################################

# create dataframe, but only by income group and broad pop proportions
pop_dat <- read_csv("data/income_region_pop_proportions_omc.csv") %>%
  mutate(middle_children = middle + children,
         old_middle = old + middle,
         all = old + middle + children) %>%
  pivot_longer(cols = c("old", "middle", "children", "old_middle", "middle_children", "all"), names_to = "target_group", values_to =  "proportion")

d_main_income_region <- left_join(pop_dat, d, by = c("income_group", "target_group")) %>%
  mutate(deaths_averted_2021 = deaths_averted_2021 / 50e6 * pop_2019,
         vaccine_n_2021 = vaccine_n_2021 / 50e6 * pop_2019,
         dapd_2021 = deaths_averted_2021 / vaccine_n_2021,
  )

write_csv(d_main_income_region, "optim_dataframes/optim_dataframe_income_omc_cov1.csv")
