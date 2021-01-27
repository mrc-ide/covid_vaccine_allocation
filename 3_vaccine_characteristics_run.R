### Plotting efficacy/coverage outputs
# Figure 2, Figure S7

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(nimue)
library(furrr)

### Load function ##############################################################
source("R/functions.R")

### Specify runs ###############################################################
# start time for transmission
t_start <- 60
vaccine_period <- 30
# transmission
R0 <- 2.5
# NPIs 2020 and 2021
Rt1 <- 1.1
Rt2 <- 2
reduction1 <- 1-Rt1/R0
reduction2 <- 1-Rt2/R0
timing1 <- 120 - t_start
timing2 <- 366 - t_start + vaccine_period + 21
# Vaccine mode of action
mode <- c("Infection", "Disease")
# Age targeting
age_target <- "1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1"
# Income group
income_group <- c("HIC", "UMIC", "LMIC", "LIC")
# Vaccine coverage
coverage <- seq(0, 1, 0.05)
# Health system constraints
hs_constraints <- c("Present", "Absent")
# Efficacy 
efficacy = seq(0.5, 1, 0.1)
# Durations of immunity
duration_R <- 365
duration_V <- 5000
dur_vacc_delay <- 7
# Vaccine start time
vaccine_start <- 366 - t_start + 21
# Seeding cases
seeding_cases <- 60
# Immunosenescence
immunosenescence <- 1
# Scenario table
scenarios <- expand_grid(t_start = t_start,
                         R0 = R0,
                         reduction1 = reduction1,
                         reduction2 = reduction2,
                         coverage = coverage,
                         mode = mode,
                         efficacy = efficacy,
                         hs_constraints = hs_constraints,
                         age_target = age_target,
                         income_group = income_group,
                         duration_R = duration_R,
                         duration_V = duration_V,
                         vaccine_period = vaccine_period,
                         dur_vacc_delay = dur_vacc_delay,
                         vaccine_start = vaccine_start,
                         timing1 = timing1,
                         timing2 = timing2,
                         immunosenescence = immunosenescence,
                         seeding_cases = seeding_cases)

nrow(scenarios)

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### Format output #############################################################
out_format <- format_out(out, scenarios)

### Save output ################################################################
saveRDS(out, "output/3_vacc_characteristics_raw.rds")
saveRDS(out_format, "output/3_vacc_characteristics.rds")
