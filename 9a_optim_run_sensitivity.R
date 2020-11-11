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
scenarios1 <- create_params_list(efficacy = 0.5) %>%
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

scenarios <- rbind(scenarios1, scenarios2, scenarios3, scenarios4, scenarios5, scenarios6)

# number the runs
scenarios$run_number <- 1:nrow(scenarios)

# include the directory to save the runs
scenarios$directory_out <- "sensitivity"

nrow(scenarios)

write_csv(scenarios, "cluster_outputs_sensitivity/scenarios.csv")
 
################################################################################
sources <- c("R/functions.R", "R/functions_cluster_sensitivity.R")
ctx <- context::context_save(path = "context",
                             sources = sources,
                             packages = c("dde", "odin", "odin.js", "squire", "nimue", "dplyr", "purrr"),
                             package_sources = provisionr::package_sources(local = c("packages/dde_1.0.2.zip",
                                                                                     "packages/odin_1.0.6.zip",
                                                                                     "packages/odin.js_0.1.8.zip",
                                                                                     "packages/squire_0.4.34.zip",
                                                                                     "packages/nimue_0.1.7.zip")))

config <- didehpc::didehpc_config(use_workers=FALSE, cluster="fi--didemrchnb")
run <- didehpc::queue_didehpc(ctx, config = config)

run$cluster_load(nodes = FALSE)

t1 <- run$enqueue_bulk(select(scenarios1, -sensitivity_run), run_scenario_cluster, do_call=TRUE, name='run_scenario', overwrite=TRUE)
