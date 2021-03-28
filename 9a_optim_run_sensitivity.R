### Runs for performing vaccine allocation #####################################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(nimue)
library(furrr)
library(readr)

### Load function ##############################################################
source("R/functions.R")
source("R/functions_optim.R")

### Specify runs ###############################################################

# Scenario table
scenarios1 <- create_params_list(efficacy = 0.7) %>%
  mutate(sensitivity_run = "reduce_efficacy")
scenarios2 <- create_params_list(immunosenescence = 0.5) %>%
  mutate(sensitivity_run = "immunosenescence")
scenarios3 <- create_params_list(mode = "Disease") %>%
  mutate(sensitivity_run = "mode_disease")
scenarios4 <- create_params_list(Rt2 = 1.5) %>%
  mutate(sensitivity_run = "lower_Rt2")
scenarios5 <- create_params_list(hs_constraints = "Absent") %>%
  mutate(sensitivity_run = "hs_constraints_absent")
scenarios6 <- create_params_list(reduce_inf = 0.5) %>%
  mutate(sensitivity_run = "reduce_inf")
scenarios7 <- create_params_list(Rt2 = 2.5) %>%
  mutate(sensitivity_run = "higher_Rt2")

scenarios <- rbind(scenarios1, scenarios2, scenarios3, scenarios4, scenarios5, scenarios6, scenarios7)

# number the runs
scenarios$run_number <- 1:nrow(scenarios)

# include the directory to save the runs
scenarios$directory_out <- "sensitivity"

nrow(scenarios)

write_csv(scenarios, "output_sensitivity/scenarios.csv")
 
#### Run the model #####################################################

# Complete the runs in batches
sr <- "lower_Rt2"

scenarios_sub <- filter(scenarios, sensitivity_run == sr)
plan(multiprocess, workers = 6)

system.time({out <- future_pmap(select(scenarios_sub, -directory_out, -sensitivity_run), run_scenario_basic, .progress = TRUE)})

write_csv(bind_rows(out), paste0("output_sensitivity/AGGREGATOR_", sr, ".csv"))
