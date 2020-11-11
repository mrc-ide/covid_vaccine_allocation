d1 <- read_csv("optim_dataframes/non_optim_allocation_income_cov1.csv") %>%
  mutate(income_group = factor(income_group, levels = c("LIC","LMIC", "UMIC", "HIC")))

d2 <- read_csv("optim_dataframes/non_optim_allocation_income_cov2.csv") %>%
  mutate(income_group = factor(income_group, levels = c("LIC","LMIC", "UMIC", "HIC"))) %>%
  filter(sensitivity_run == "default") %>%
  select(-sensitivity_run)


# create a summary table
d_summ <- rbind(d1, d2) %>%
  mutate(dap100d_2021 = round(dapd_2021 * 100, 3),
    global_pop = 7646445824,
         deaths_averted_per_m = round(deaths_averted_2021 / pop_2019 * 1e6),
         total_deaths_averted_per_m = round(total_deaths_averted / global_pop * 1e6),
         dap100d_total = round(total_deaths_averted / total_vaccine_n * 100, 3)) %>%
  select(strategy, income_group, age_target, deaths_averted_per_m, dap100d_2021, total_deaths_averted_per_m, dap100d_total)

# save summary table
write_csv(d_summ, "tables/non_optim_results_summary_table_2_S2.csv")

# sensitivity analysis table - table S4
d_table <- read_csv("optim_dataframes/non_optim_allocation_income_cov2.csv") %>%
  mutate(income_group = factor(income_group, levels = c("LIC","LMIC", "UMIC", "HIC"))) %>%
  mutate(dap100d_2021 = round(dapd_2021 * 100, 3),
         global_pop = 7646445824,
         deaths_averted_per_m = round(deaths_averted_2021 / pop_2019 * 1e6),
         total_deaths_averted_per_m = round(total_deaths_averted / global_pop * 1e6),
         dap100d_total = round(total_deaths_averted / total_vaccine_n * 100, 3)) %>%
  select(sensitivity_run, strategy, income_group, target_group, deaths_averted_per_m, dap100d_2021, total_deaths_averted_per_m, dap100d_total)

# supplementary table - all details
write_csv(d_table, "tables/non_optim_results_summary_sensitivity.csv")

# table s4
tab_s4 <- d_table %>%
  select(strategy, sensitivity_run, total_deaths_averted_per_m, dap100d_total) %>%
  unique() %>%
  mutate(measure = paste0(total_deaths_averted_per_m, " (", dap100d_total, ")")) %>%
  arrange(strategy) %>%
  select(strategy, sensitivity_run, measure) %>%
  pivot_wider(names_from = "sensitivity_run", id_cols = "strategy", values_from = "measure")

write_csv(tab_s4, "tables/table_s4.csv")

