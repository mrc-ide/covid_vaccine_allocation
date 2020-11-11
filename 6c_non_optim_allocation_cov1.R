### Create dataframe for the allocation strategies without optimisation ########
# see data/vaccine_allocation_strategies_income_group.xlsx for scenario workings
# 15% wastage and buffer applied to the total available doses

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(gtools)
################################################################################

### Load function ##############################################################
source("R/functions_optim.R")

### Read dataframe from 6b #####################################################
d_main <- read_csv("optim_dataframes/optim_dataframe_income_omc_cov1.csv")
### Read strategies ############################################################
strategies <- read_csv("data/non_optim_scenarios_cov1.csv", col_types = "ccccdddd") %>% filter(!(coverage_children == 0 & coverage_middle == 0 & coverage_old == 0))

# Select immunosenescence
imm <- 1
# Select mode
m <- "Infection"
# hs constraints
hs_con <- "Present"
# immunity
d_R <- 365
d_V <- 5000

# Filter to required scenario combination
d <- d_main %>%
  filter(immunosenescence == imm,
         mode == m,
         hs_constraints == hs_con,
         duration_V == d_V,
         duration_R == d_R) %>%
filter(!(coverage_children == 0 & coverage_middle == 0 & coverage_old == 0))

# 1. All countries receive doses in proportion to population - 2 dose schedule
strategy1 <- filter(strategies, strategy == "strategy1_cov1")
d1 <- strategy_cov1_fn(strategy1, d)

# 2. All countries receive doses in proportion to population - 2 dose schedule - allocated to elderly first, then middle
strategy2 <- filter(strategies, strategy == "strategy2_cov1")
d2 <- strategy_cov1_fn(strategy2, d)

# 3. All countries receive doses in proportion to size of population over 65, allocated to 65+ first, then middle group - 2 dose schedule
strategy3 <- filter(strategies, strategy == "strategy3_cov1")
d3 <- strategy_cov1_fn(strategy3, d)

# 4. High income first - 1b dose constraint ie 2 dose schedule
strategy4 <- filter(strategies, strategy == "strategy4_cov1")
d4 <- strategy_cov1_fn(strategy4, d)

# 5. low income first - 1b dose constraint ie 2 dose schedule
strategy5 <- filter(strategies, strategy == "strategy5_cov1")
d5 <- strategy_cov1_fn(strategy5, d)

# 6. All countries receive doses in proportion to population, plus additional 1.15 doses to HIC and 1.1 doses to MIC - 2 dose schedule
strategy6 <- filter(strategies, strategy == "strategy6_cov1")
d6 <- strategy_cov1_fn(strategy6, d)

out <- rbind(d1, d2, d3, d4, d5, d6) %>%
  select(-doses_available)

write_csv(out, "optim_dataframes/non_optim_allocation_income_cov1.csv")
