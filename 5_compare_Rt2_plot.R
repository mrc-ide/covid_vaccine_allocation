#################################################################
# 90% vaccine efficacy, 80% coverage, 4 income settings
# show life years saved and deaths averted as bar chart -  for two levels of Rt2

### Load packages ###############################################################
library(dplyr)
library(ggplot2)
library(patchwork)

### Load functions #############################################################
source("R/functions.R")

### Plotting function ##########################################################
plotfunc <- function(df, y, eff, Rt1_val, Rt2_val, ylim, caption) {
  dat <- filter(df, name == y, efficacy == eff, Rt1 == Rt1_val, Rt2 == Rt2_val)
  ggplot(data = dat, aes(x = income_group, y = value/50e6*1e3, fill = hs_constraints)) +
    geom_col(position = "dodge") +
    xlab("Income setting") +
    ylab(paste0(y," per thousand")) +
    labs(x = "Income setting", y = paste0(y," per thousand"), fill = "Health system constraints", title = caption, parse = T) +
    lims(y = c(0, ylim)) +
    scale_fill_manual(values = c("grey25", "grey70")) +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.line = element_line(),
          plot.caption = element_text(hjust = 0.5))
}

### Create plot ################################################################
# Load data
d1 <- readRDS("output/5_compare_Rt2.rds") %>%
  mutate(efficacy = factor(efficacy*100),
         income_group = factor(income_group, levels = c("LIC","LMIC", "UMIC", "HIC")),
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0) %>%
  filter(coverage == 0.8,
         duration_R == 365,
         mode == "Infection") %>%
  select(income_group, hs_constraints, efficacy, Rt1, Rt2, deaths_averted_2021, years_life_saved_2021) %>%
  rename("Deaths averted" = "deaths_averted_2021", "Life-years gained" = "years_life_saved_2021") %>%
  pivot_longer(c("Deaths averted", "Life-years gained"))

g1 <- plotfunc(d1, "Deaths averted", eff = 90, Rt1_val = 1.1, Rt2_val = 2.5, ylim = 4.5, caption = bquote("R"[t2]==2.5))
g2 <- plotfunc(d1, "Life-years gained", eff = 90, Rt1_val = 1.1, Rt2_val = 2.5, ylim = 100, caption = bquote("R"[t2]==2.5))
g3 <- plotfunc(d1, "Deaths averted", eff = 90, Rt1_val = 1.1, Rt2_val = 2, ylim = 4.5, caption = bquote("R"[t2]==2))
g4 <- plotfunc(d1, "Life-years gained", eff = 90, Rt1_val = 1.1, Rt2_val = 2, ylim = 100, caption = bquote("R"[t2]==2))
g5 <- plotfunc(d1, "Deaths averted", eff = 90, Rt1_val = 1.1, Rt2_val = 1.5, ylim = 4.5, caption = bquote("R"[t2]==1.5))
g6 <- plotfunc(d1, "Life-years gained", eff = 90, Rt1_val = 1.1, Rt2_val = 1.5, ylim = 100, caption = bquote("R"[t2]==1.5))

###################################################################
# combine plots

all_plots <- (g1 | g2 | g3 | g4 | g5 | g6) + plot_layout(guide = "collect", ncol = 2, nrow = 3)+ plot_annotation(tag_levels = 'A')

all_plots

ggsave("plots/FigS19.png", all_plots, height = 8, width = 7.5)
