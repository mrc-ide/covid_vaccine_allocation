### Create dataframe for the fixed allocation ###########################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

### Load outputs ###############################################################
d <- readRDS("output/6_non_optim_cov1.rds") %>%
  select(t_start, R0, reduction1, reduction2, coverage, age_target, income_group, immunosenescence, mode, hs_constraints, efficacy, duration_R, duration_V, vaccine_start, seeding_cases, timing1, timing2, deaths_averted_2021, vaccine_n_2021) %>%
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
         all = old + middle + children) %>%
  pivot_longer(cols = c("old", "middle", "children", "old_middle", "middle_children", "all"), names_to = "target_group", values_to =  "proportion")

d_main_income_region <- left_join(pop_dat, d, by = c("income_group", "target_group")) %>%
  mutate(deaths_averted_2021 = deaths_averted_2021 / 50e6 * pop_2019,
         vaccine_n_2021 = vaccine_n_2021 / 50e6 * pop_2019,
         dapd_2021 = deaths_averted_2021 / vaccine_n_2021,
  )

# include the "none" option
d_none <- filter(d_main_income_region, target_group == "old") %>%
  mutate(age_target = "none",
         proportion = 0,
         deaths_averted_2021 = 0,
         vaccine_n_2021 = 0,
         dapd_2021 = 0)

d_main_income_region <- rbind(d_main_income_region, d_none)

write_csv(d_main_income_region, "optim_dataframes/optim_dataframe_income_omc.csv")
