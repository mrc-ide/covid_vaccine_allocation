### Figure 4 ###########################

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

df1 <- read_csv("optim_dataframes/fig_4_inputs.csv")

### Plotting params ############################################################

# Age targeting labels
ag <- paste0(seq(0, 80, 5), "+")
# Income group colours
cols <- RColorBrewer::brewer.pal(12, "Paired")[c(2, 4, 6, 10)]
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

all_plots <- (g1 | g2 | g3 | g4 | g5 | g6 | g7 | g8) + plot_layout(guide = "collect", ncol = 2, nrow = 4, heights = c(1, 1, 1, 1))+ plot_annotation(tag_levels = 'A')

all_plots

ggsave("plots/Fig4.png", all_plots, height = 11, width = 10)
