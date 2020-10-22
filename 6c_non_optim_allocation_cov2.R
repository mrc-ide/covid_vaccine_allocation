### Create dataframe for the allocation strategies without optimisation ########
# see data/vaccine_allocation_strategies_income_group.xlsx for scenario workings

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

### Load outputs ###############################################################
d <- readRDS("output/6_non_optim_cov2.rds") %>%
  select(strategy, t_start, R0, reduction1, reduction2, coverage, age_target, income_group, immunosenescence, mode, hs_constraints, efficacy, duration_R, duration_V, vaccine_start, seeding_cases, timing1, timing2, deaths_averted_2021, vaccine_n_2021) %>%
  filter(coverage > 0) %>%
  mutate(target_group = "")

d[which(d$age_target == "0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1"),]$target_group <- "old"
d[which(d$age_target == "0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0"),]$target_group <- "middle"
d[which(d$age_target == "1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0"),]$target_group <- "children"
d[which(d$age_target == "0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1"),]$target_group <- "old_middle"
d[which(d$age_target == "1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0"),]$target_group <- "middle_children"
d[which(d$age_target == "1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1"),]$target_group <- "all"

################################################################################

# create dataframe, but only by income group and broad pop proportions
pop_dat <- read_csv("data/income_region_pop_proportions_omc.csv") %>%
  mutate(middle_children = middle + children,
         old_middle = old + middle,
         all = old + middle + children,
         none = 0) %>%
  pivot_longer(cols = c("old", "middle", "children", "old_middle", "middle_children", "all", "none"), names_to = "target_group", values_to =  "proportion")

d_main_income_region <- left_join(d, pop_dat, by = c("income_group", "target_group")) %>%
  mutate(deaths_averted_2021 = deaths_averted_2021 / 50e6 * pop_2019,
         vaccine_n_2021 = vaccine_n_2021 / 50e6 * pop_2019,
         dapd_2021 = deaths_averted_2021 / vaccine_n_2021,
  ) %>%
  group_by(strategy, immunosenescence, mode, hs_constraints, efficacy) %>%
  mutate(total_vaccine_n = sum(vaccine_n_2021),
         total_deaths_averted = sum(deaths_averted_2021))

write_csv(d_main_income_region, "optim_dataframes/non_optim_allocation_income_cov2.csv")
