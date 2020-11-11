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
dat <- read_csv("data/non_optim_scenarios_cov2.csv")
sens <- read_csv("data/non_optim_scenarios_cov2_sensitivity.csv")
sensitivity_run <- unique(sens$sensitivity_run)
strategy <- unique(dat$strategy)
income_group <- unique(dat$income_group)
df <- expand_grid(sensitivity_run, strategy, income_group)
scenarios <- df %>%
  left_join(dat) %>%
  left_join(sens) %>%
  na.omit()

# Age-based coverage vector
scenarios$varying_coverage <- 1
scenarios <- left_join(scenarios, dat)

nrow(scenarios)

################################################################################

#### Run the model #############################################################
plan(multiprocess, workers = 6)
system.time({out <- future_pmap(select(scenarios, -strategy, -target_group, -sensitivity_run), run_scenario, .progress = TRUE)})
################################################################################

#### Format output #############################################################
out_format <- format_out(out, scenarios)
################################################################################

### Save output ################################################################
saveRDS(out, "output/6_non_optim_cov2_raw.rds")
saveRDS(out_format, "output/6_non_optim_cov2.rds")
################################################################################