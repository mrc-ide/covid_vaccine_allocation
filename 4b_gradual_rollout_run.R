### Figure S9 runs ##############################################################

### Load packages ##############################################################
# devtools::install_github("mrc-ide/nimue@develop", force = TRUE)
library(dplyr)
library(purrr)
library(tidyr)
library(nimue)
library(furrr)

### Load functions #############################################################
source("R/functions.R")

### Specify runs ###############################################################
# start time for transmission
t_start <- 60
vaccine_period <- 30
# transmission
R0 <- 2.5
# NPIs 2020 and 2021
Rt1 <- 1.1
Rt2 <- c(1.1, 1.3, 1.5, 2, 2.5)
timing1 <- 120 - t_start
timing2 <- 366 - t_start + vaccine_period + 21
# Vaccine coverage
coverage <- c(0,0.8) 
# Mode of action
mode <- c("Infection")
# Health system constraints
hs_constraints <- "Present"
# Efficacy 
efficacy <- c(0.7,0.9)
# Vaccine target age
age_target <- NA
vaccine_coverage_mat <- c("All", "Elderly", "Working Elderly Children")
# Income group
income_group <- c("HIC")
# Durations of immunity
duration_R <- 365
duration_V <- 5000
dur_vacc_delay <- 7
# Vaccine start time
vaccine_start <- 366 - t_start + 21
# Vaccine rollout period
vaccine_period <- c(30, 365)
# Seeding cases
seeding_cases <- 60
# immunosenescence
immunosenescence <- 1
# Scenario table
scenarios <- expand_grid(t_start = t_start,
                         R0 = R0,
                         Rt1 = Rt1,
                         Rt2 = Rt2,
                         coverage = coverage,
                         mode = mode,
                         efficacy = efficacy,
                         hs_constraints = hs_constraints,
                         age_target = age_target,
                         vaccine_coverage_mat = vaccine_coverage_mat,
                         income_group = income_group,
                         duration_R = duration_R,
                         duration_V = duration_V,
                         dur_vacc_delay = dur_vacc_delay,
                         vaccine_start = vaccine_start,
                         vaccine_period = vaccine_period,
                         timing1 = timing1,
                         timing2 = timing2,
                         immunosenescence = immunosenescence,
                         seeding_cases = seeding_cases)

scenarios <- scenarios %>%
  mutate(reduction1 = 1-Rt1/R0,
         reduction2 = 1-Rt2/R0) %>%
  select(-Rt1, -Rt2)
nrow(scenarios)

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})

#### Format output #############################################################
out_format <- format_out(out, scenarios)

### Save output ################################################################
saveRDS(out, "output/4_gradual_rollout_raw.rds")
saveRDS(out_format, "output/4_gradual_rollout.rds")
