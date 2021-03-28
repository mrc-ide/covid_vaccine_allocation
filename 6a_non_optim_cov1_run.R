### Runs for performing vaccine allocation######################################
# cov1 represents where limited countries allocated at high coverage
### Load packages ##############################################################
library(tidyverse)
library(purrr)
library(nimue)
library(furrr)

### Load function ##############################################################
source("R/functions.R")

### Specify runs ###############################################################
dat <- read_csv("data/non_optim_scenarios_cov1.csv")
dat$run_number <- 1:nrow(dat)

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
# Immunosenescence (efficacy in 65+ relative to efficacy)
immunosenescence <- 1
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
# Reduce infectiousness in under 10 years
reduce_inf <- 1
# Run number 
run_number <- 1:nrow(dat)

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
                         dur_vacc_delay = dur_vacc_delay,
                         vaccine_start = vaccine_start,
                         vaccine_period = vaccine_period,
                         seeding_cases = seeding_cases,
                         timing1 = timing1,
                         timing2 = timing2,
                         reduce_inf = reduce_inf,
                         run_number = run_number
)

scenarios <- left_join(scenarios, dat) %>%
  select(-run_number, -doses_available)
scenarios$varying_coverage <- 1

nrow(scenarios)
################################################################################

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(select(scenarios, -target_group, -strategy), run_scenario, .progress = TRUE)})
################################################################################

#### Format output #############################################################
out_format <- format_out(out, scenarios)
################################################################################

### Save output ################################################################
saveRDS(out, "output/6_non_optim_cov1_raw.rds")
saveRDS(out_format, "output/6_non_optim_cov1.rds")
################################################################################