### Counterfactuals ############################################################

### Load packages ##############################################################
# devtools::install_github("mrc-ide/nimue@prioritisation_matrices")
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
t_start = 60
# transmission
R0 <- c(2.5, 3.0)
# NPIs 2020 and 2021
Rt1 <- c(0.6, 0.8, 1, 1.1)
Rt2 <- c(1.3, 1.5, 2)
timing1 <- 120 - t_start
timing2 <- 365 + 30 - t_start
# Vaccine coverage
coverage <- 0
# Vaccine start
vaccine_start <- 365 - t_start
# Income group
income_group <- c("HIC", "UMIC", "LMIC", "LIC")
# Durations of immunity
duration_R <- c(365, Inf)
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
                         timing1 = timing1,
                         timing2 = timing2,
                         vaccine_start = vaccine_start,
                         seeding_cases = seeding_cases)

scenarios <- scenarios %>%
  mutate(reduction1 = 1-Rt1/R0,
         reduction2 = 1-Rt2/R0) %>%
  select(t_start, R0, reduction1, reduction2, coverage, income_group, duration_R, timing1, timing2, seeding_cases)

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
