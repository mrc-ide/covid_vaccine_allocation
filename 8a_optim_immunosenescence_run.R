### Runs for performing between-country vaccine allocation #####################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(nimue)
library(furrr)
library(readr)

### Load functions ##############################################################
source("R/functions.R")
source("R/functions_optim.R")

### Specify parameter set #######################################################
sensitivity_run <- "default_immunosenescence"

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
# Age targeting
age_target <- c("0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0_0", read_csv("data/age_target_strategy_old.csv")$age_target)
# Income group
income_group <- c("HIC", "UMIC", "LMIC", "LIC")
# Immunosenescence (efficacy in 65+ relative to efficacy)
immunosenescence <- 0.5
# Vaccine coverage
coverage <- 0.8
# Vaccine mode of action
mode <- "Combined"
# Health system constraints
hs_constraints <- "Present"
# Efficacy
efficacy <- 0.9
# Durations of immunity
duration_R <- 365
duration_V <- 5000
dur_vacc_delay <- 7
# Vaccine start time
vaccine_start <- 366 - t_start + 21
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
                         dur_vacc_delay = dur_vacc_delay,
                         vaccine_start = vaccine_start,
                         vaccine_period = vaccine_period,
                         seeding_cases = seeding_cases,
                         timing1 = timing1,
                         timing2 = timing2
)

# number the runs
scenarios$run_number <- 1:nrow(scenarios)

# include the directory to save the runs
scenarios$directory_out <- "default_immunosenescence"

nrow(scenarios)

write_csv(scenarios, paste0("output_", sensitivity_run, "/scenarios.csv"))

#### Run the model - not on the cluster #########################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(select(scenarios, -directory_out), run_scenario_basic, .progress = TRUE)})

write_csv(bind_rows(out), paste0("output_", sensitivity_run,"/AGGREGATOR.csv"))
