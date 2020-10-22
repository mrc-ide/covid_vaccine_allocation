library(readr)
library(tidyr)
library(dplyr)
library(stringr)

d <- read_csv("between_country_optim_outputs/optimal.csv")
pop_dat <- read_csv("data/income_region_pop_proportions_fine.csv")

##################################################################
# look at optimal solution
d_summary <- d %>%
  select(income_group, pop_2019, income_group_size, partition_size, age_target, cost, y) %>%
  rename("doses" = "cost",
         "deaths_averted" = "y") %>%
  group_by(income_group, age_target, pop_2019, income_group_size) %>%
  summarise(partition_size = sum(partition_size),
            doses = sum(doses),
            deaths_averted = sum(deaths_averted)) %>%
  ungroup() %>%
  mutate(prop_doses = round(doses/sum(doses)*100,1)) %>%
  left_join(pop_dat) %>%
  mutate(proportion_pop = 0) %>%
  arrange(-prop_doses)

for (i in 1:nrow(d_summary)){
  proportion <- as.numeric(unlist(str_split(d_summary[i,2], "_")))
  d_summary[i,]$proportion_pop <- sum(d_summary[i,9:25]*proportion)
}

d_summary <- d_summary %>%
  select(income_group, age_target, pop_2019, partition_size, income_group_size, doses, deaths_averted, prop_doses, proportion_pop) %>%
  mutate(coverage = round((partition_size / income_group_size * 0.8)*100,1),
         dap100fvp = round(deaths_averted / doses, 3)) %>%
  select(-pop_2019, -proportion_pop)

d_summary

# also look at impact within each income group
d_summary_icg <- d_summary %>%
  group_by(income_group, income_group_size) %>%
  summarise(partition_size = sum(partition_size),
            doses = sum(doses),
            deaths_averted = sum(deaths_averted),
            prop_doses = sum(prop_doses)) %>%
  ungroup() %>%
  mutate(doses_per_pop = round(doses/income_group_size*100,1)) %>%
  mutate(dap100fvp = round(deaths_averted/doses*100,3),
         deaths_averted_per_m = round(deaths_averted/income_group_size * 1e6),
         total_deaths_averted_per_m = round(sum(deaths_averted)/sum(income_group_size) * 1e6),
         dap100fvp_total = round(sum(deaths_averted)/sum(doses)*100,3)) %>%
  mutate(income_group = factor(income_group, levels = c("HIC", "UMIC", "LMIC", "LIC"))) %>%
  arrange(income_group) %>%
  select(income_group, doses, prop_doses, doses_per_pop, deaths_averted_per_m, dap100fvp, total_deaths_averted_per_m, dap100fvp_total)

##################################################################
# Save tables
##################################################################
#write_csv(d_summary, "tables/optimal_strategy_between_country.csv")
write_csv(d_summary_icg, "tables/optimal_strategy_between_country_icg.csv")
write_csv(select(d_summary_icg, income_group, deaths_averted_per_m, dap100fvp, total_deaths_averted_per_m, dap100fvp_total), "tables/table_2_optimal.csv")
