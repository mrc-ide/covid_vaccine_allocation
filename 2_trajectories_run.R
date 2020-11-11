
### Load packages ##############################################################
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
R0 <- 2.5
# NPIs 2020 and 2021
Rt1 <- 1.1
Rt2 <- 2
reduction1 <- 1-Rt1/R0
reduction2 <- 1-Rt2/R0
timing1 <- 120 - t_start
timing2 <- 366 - t_start + 30
# Vaccine coverage
coverage <- c(0,0.8)
# Mode of action
mode <- c("Infection")
# Health system constraints{}
hs_constraints <- c("Present")
# Efficacy 
efficacy <- 0.7
# Age targeting
age_target <- "1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1"
# Income group
income_group <- c("HIC", "UMIC", "LMIC", "LIC")
# Durations of immunity
duration_R <- c(Inf, 365, 183)
duration_V <- c(5000, 365, 183)
# Vaccine start time
vaccine_start <- 366 - t_start
# Immunoscenescence
immunosenescence <- 1
# Seeding cases
seeding_cases <- 60

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
                         vaccine_start = vaccine_start,
                         timing1 = timing1,
                         timing2 = timing2,
                         immunosenescence = immunosenescence,
                         seeding_cases = seeding_cases)

nrow(scenarios)

################################################################################

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(scenarios, run_scenario, .progress = TRUE)})
################################################################################

#### Format output #############################################################
out_format <- format_out(out, scenarios)
################################################################################

### Save output ################################################################
saveRDS(out, "output/2_trajectories_raw.rds")
saveRDS(out_format, "output/2_trajectories.rds")
################################################################################