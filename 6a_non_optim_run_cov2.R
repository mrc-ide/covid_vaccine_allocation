### Runs for performing vaccine allocation######################################
# cov2 represents the supply allocated across all countries at reduced coverage
### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(nimue)
library(furrr)
library(readr)
################################################################################

### Load function ##############################################################
source("R/functions.R")
################################################################################

### Specify runs ###############################################################
# start time for transmission
t_start = 60
# transmission
R0 <- 2.5
# NPIs 2020 and 2021
Rt1 <- 1
Rt2 <- 2
reduction1 <- 1-Rt1/R0
reduction2 <- 1-Rt2/R0
timing1 <- 120 - t_start
timing2 <- 366 - t_start + 30
# Immunosenescence (efficacy in 65+ relative to efficacy)
immunosenescence <- 1
# Vaccine mode of action
mode <- c("Infection")
# Health system constraints
hs_constraints <- c("Present")
# Efficacy
efficacy <- 0.7
# Durations of immunity
duration_R <- Inf
duration_V <- 5000
# Vaccine start time
vaccine_start <- 366 - t_start
# Seeding cases
seeding_cases <- 60
# Scenario number
run_number <- 1:52
# Scenario table
scenarios <- expand_grid(t_start = t_start,
                         R0 = R0,
                         reduction1 = reduction1,
                         reduction2 = reduction2,
                         immunosenescence = immunosenescence,
                         mode = mode,
                         hs_constraints = hs_constraints,
                         efficacy = efficacy,
                         duration_R = duration_R,
                         duration_V = duration_V,
                         vaccine_start = vaccine_start,
                         seeding_cases = seeding_cases,
                         timing1 = timing1,
                         timing2 = timing2,
                         run_number = run_number
)

scenarios <- left_join(scenarios, read_csv("data/non_optim_scenarios_cov2.csv")) %>%
  select(-run_number, -target_group)

nrow(scenarios)
################################################################################

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(select(scenarios, -strategy), run_scenario, .progress = TRUE)})
################################################################################

#### Format output #############################################################
out_format <- format_out(out, scenarios)
################################################################################

### Save output ################################################################
saveRDS(out, "output/6_non_optim_cov2_raw.rds")
saveRDS(out_format, "output/6_non_optim_cov2.rds")
################################################################################