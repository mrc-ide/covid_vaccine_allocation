### Load packages ##############################################################
# devtools::install_github("mrc-ide/nimue@develop", force = TRUE)
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
Rt1 <- c(1, 1.1)
Rt2 <- c(1.5, 2)
reduction1 <- 1-Rt1/R0
reduction2 <- 1-Rt2/R0
timing1 <- 120 - t_start
timing2 <- 366 - t_start + vaccine_period + 21
# Vaccine mode of action
mode <- "Infection"
# Age targeting
age_target <- "1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1"
# Income group
income_group <- c("HIC", "UMIC", "LMIC", "LIC")
# Vaccine coverage
coverage <- c(0,0.8)
# Health system constraints
hs_constraints <- c("Present", "Absent")
# Efficacy 
efficacy <- c(0.7, 0.9)
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
                         age_target = age_target,
                         income_group = income_group,
                         immunosenescence = immunosenescence,
                         mode = mode,
                         hs_constraints = hs_constraints,
                         efficacy = efficacy,
                         duration_R = duration_R,
                         duration_V = duration_V,
                         dur_vacc_delay = dur_vacc_delay,
                         vaccine_start = vaccine_start,
                         vaccine_period = vaccine_period,
                         seeding_cases = seeding_cases,
                         timing1 = timing1,
                         timing2 = timing2)

nrow(scenarios)

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### Format output #############################################################
out_format <- format_out(out, scenarios)

### Save output ################################################################
saveRDS(out, "output/5_compare_Rt2_raw.rds")
saveRDS(out_format, "output/5_compare_Rt2.rds")
