### Runs for performing vaccine allocation######################################
# cov1 represents where limited countries allocated at high coverage
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
Rt1 <- 1
Rt2 <- 2
reduction1 <- 1-Rt1/R0
reduction2 <- 1-Rt2/R0
timing1 <- 120 - t_start
timing2 <- 366 - t_start + 30
# Age targeting
age_target <- c("0_0_0_0_0_0_0_0_0_0_0_0_0_1_1_1_1",
                "0_0_0_1_1_1_1_1_1_1_1_1_1_0_0_0_0",
                "1_1_1_0_0_0_0_0_0_0_0_0_0_0_0_0_0",
                "0_0_0_1_1_1_1_1_1_1_1_1_1_1_1_1_1",
                "1_1_1_1_1_1_1_1_1_1_1_1_1_0_0_0_0",
                "1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1_1")
# Income group
income_group <- c("HIC", "UMIC", "LMIC", "LIC")
# Immunosenescence (efficacy in 65+ relative to efficacy)
immunosenescence <- 1
# Vaccine coverage
coverage <- c(0,0.8)
# Coverage vector
#coverage_vect <- c("0_0_0_")
# Vaccine mode of action
mode <- "Infection"
# Health system constraints
hs_constraints <- "Present"
# Efficacy
efficacy <- 0.7
# Durations of immunity
duration_R <- Inf
duration_V <- 5000
# Vaccine start time
vaccine_start <- 366 - t_start
# Seeding cases
seeding_cases <- 60

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
                         vaccine_start = vaccine_start,
                         seeding_cases = seeding_cases,
                         timing1 = timing1,
                         timing2 = timing2
)

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
saveRDS(out, "output/6_non_optim_cov1_raw.rds")
saveRDS(out_format, "output/6_non_optim_cov1.rds")
################################################################################