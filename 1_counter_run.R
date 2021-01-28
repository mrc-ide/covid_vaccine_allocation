### Counterfactuals ############################################################
# Figure S5, Figure S6, Figure 1

### Load packages ##############################################################
# devtools::install_github("mrc-ide/nimue")
library(dplyr)
library(purrr)
library(tidyr)
library(nimue)
library(furrr)
################################################################################

### Load function ##############################################################
source("R/functions.R")
################################################################################

### Specify runs ###############################################################
# start time for transmission
t_start <- 60
vaccine_period <- 30
# transmission
R0 <- c(2.5, 3.0)
# NPIs 2020 and 2021
Rt1 <- c(0.9, 1.1, 1.3)
Rt2 <- c(1.5, 2, 2.5)
timing1 <- 120 - t_start
timing2 <- 366 - t_start + vaccine_period + 21
# Vaccine coverage
coverage <- 0
# Vaccine start
vaccine_start <- 366 - t_start + 21
# Income group
income_group <- c("HIC", "UMIC", "LMIC", "LIC")
# Durations of immunity
duration_R <- c(365, Inf)
dur_vacc_delay <- 7
# Seeding cases
seeding_cases <- 60
# Scenario table
scenarios <- expand_grid(t_start = t_start,
                         R0 = R0,
                         Rt1 = Rt1,
                         Rt2 = Rt2,
                         coverage = coverage,
                         income_group = income_group,
                         duration_R = duration_R,
                         vaccine_period = vaccine_period,
                         dur_vacc_delay = dur_vacc_delay,
                         vaccine_start = vaccine_start,
                         timing1 = timing1,
                         timing2 = timing2,
                         seeding_cases = seeding_cases)

scenarios <- scenarios %>%
  mutate(reduction1 = 1-Rt1/R0,
         reduction2 = 1-Rt2/R0) %>%
  select(t_start, R0, reduction1, reduction2, coverage, income_group, duration_R, vaccine_period, dur_vacc_delay, vaccine_start, timing1, timing2, seeding_cases)

nrow(scenarios)

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})
################################################################################

#### Format output #############################################################
out_format <- format_out(out, scenarios)
################################################################################

### Save output ################################################################
saveRDS(out, "output/1_counter_raw.rds")
saveRDS(out_format, "output/1_counter.rds")
################################################################################
