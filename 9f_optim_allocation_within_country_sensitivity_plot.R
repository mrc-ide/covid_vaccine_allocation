### Figure 4 - supplementary figures ###########################

### Load packages ##############################################################
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(stringr)
library(data.table)
library(patchwork)
library(ggplot2)

### Plotting functions #########################################################

source("R/age_grid_plot_functions.R")

### Load outputs ###############################################################
sensitivity_run <- "immunosenescence"# lower_Rt2 hs_constraints_absent" #"mode_disease" #"reduce_efficacy" #immunosenescence
df1 <- read_csv(paste0("optim_dataframes/fig_4_inputs_", sensitivity_run, ".csv"))

if (sensitivity_run == "reduce_efficacy"){sensitivity_run_text = "Vaccine efficacy 50%"}
if (sensitivity_run == "immunosenescence"){sensitivity_run_text = "Immunosenescence"}
if (sensitivity_run == "mode_disease"){sensitivity_run_text = "Disease-blocking vaccine"}
if (sensitivity_run == "lower_Rt2"){sensitivity_run_text = "Higher level of continued NPIs"}
if (sensitivity_run == "hs_constraints_absent"){sensitivity_run_text = "Health system constraints absent"}

### Plotting params ############################################################

# Age bands
x <- seq(0,80,5)
y <- seq(4,84,5)
z <- paste0(x,"-",y)
z[17] <- "80+"

################################################################################

g1 <- plot1(get_ageplot_df(df1, "HIC"), caption = "High-income", ylabs = z)
g3 <- plot1(get_ageplot_df(df1, "UMIC"), caption = "Upper-middle-income", ylabs = z)
g5 <- plot1(get_ageplot_df(df1, "LMIC"), caption = "Lower-middle-income", ylabs = z)
g7 <- plot1(get_ageplot_df(df1, "LIC"), caption = "Low-income", ylabs = z)

g2 <- plot2(filter(df1, income_group == "HIC"))
g4 <- plot2(filter(df1, income_group == "UMIC"))
g6 <- plot2(filter(df1, income_group == "LMIC"))
g8 <- plot2(filter(df1, income_group == "LIC"))

################################################################################

# combine plots and save
all_plots <- (g1 | g2 | g3 | g4 | g5 | g6 | g7 | g8) + plot_layout(guide = "collect", ncol = 2, nrow = 4, heights = c(1, 1, 1, 1))+ plot_annotation(tag_levels = 'A') +
  plot_annotation(title = paste0("Sensitivity analysis: ", sensitivity_run_text))

all_plots

ggsave(paste0("plots/Fig4_", sensitivity_run,".png"), all_plots, height = 11, width = 10)
