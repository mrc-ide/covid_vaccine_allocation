### Table 3 ###########################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(stringr)
library(data.table)
library(patchwork)
library(ggplot2)

### Load outputs ###############################################################

# Supplementary table
df1 <- read_csv("tables/optimal_strategy_between_country_icg.csv") %>%
  mutate(strategy = "default")
df2 <- read_csv("tables/optimal_strategy_between_country_icg_reduce_efficacy.csv") %>%
  mutate(strategy = "reduce_efficacy")
df3 <- read_csv("tables/optimal_strategy_between_country_icg_immunosenescence.csv") %>%
  mutate(strategy = "immunosenescence")
df4 <- read_csv("tables/optimal_strategy_between_country_icg_mode_disease.csv") %>%
  mutate(strategy = "mode_disease")
df5 <- read_csv("tables/optimal_strategy_between_country_icg_lower_Rt2.csv") %>%
  mutate(strategy = "lower_Rt2")
df6 <- read_csv("tables/optimal_strategy_between_country_icg_higher_Rt2.csv") %>%
  mutate(strategy = "higher_Rt2")
df7 <- read_csv("tables/optimal_strategy_between_country_icg_hs_constraints_absent.csv") %>%
  mutate(strategy = "hs_constraints_absent")
df8 <- read_csv("tables/optimal_strategy_between_country_icg_reduce_inf.csv") %>%
  mutate(strategy = "reduce_inf")
df9 <- read_csv("tables/optimal_strategy_between_country_icg_life_years.csv") %>%
  mutate(strategy = "life_years")

df_all <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9)

tab_S4 <- df_all %>%
  select(-doses_per_pop, -prop_doses, -doses)

write_csv(tab_S4, "tables/table_S4.csv")

# table 3
tab_3 <- df_all %>%
  mutate(measure = paste0(round(doses_per_pop, 1), "% (", round(prop_doses,1), "%)")) %>%
  select(income_group, strategy, measure) %>%
  pivot_wider(id_cols = "income_group", names_from = "strategy", values_from = "measure")

tab_3 <- df_all %>%
  mutate(measure = paste0(round(doses_per_pop, 1), "% (", round(prop_doses,1), "%)")) %>%
  select(income_group, strategy, measure) %>%
  pivot_wider(names_from = "income_group", id_cols = "strategy", values_from = "measure")

write_csv(tab_3, "tables/table_3.csv")
