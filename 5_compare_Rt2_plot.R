#################################################################
# 70% vaccine efficacy, 80% coverage, 4 income settings
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

g1 <- plotfunc(d1, "Deaths averted", eff = 90, Rt1_val = 1.1, Rt2_val = 2, ylim = 3.5, caption = bquote("R"[t2]==2))
g2 <- plotfunc(d1, "Life-years gained", eff = 90, Rt1_val = 1.1, Rt2_val = 2, ylim = 90, caption = bquote("R"[t2]==2))
g3 <- plotfunc(d1, "Deaths averted", eff = 90, Rt1_val = 1.1, Rt2_val = 1.5, ylim = 3.5, caption = bquote("R"[t2]==1.5))
g4 <- plotfunc(d1, "Life-years gained", eff = 90, Rt1_val = 1.1, Rt2_val = 1.5, ylim = 90, caption = bquote("R"[t2]==1.5))

###################################################################
# combine plots

all_plots <- (g1 | g2 | g3 | g4 ) + plot_layout(guide = "collect", ncol = 2, nrow = 2)+ plot_annotation(tag_levels = 'A')

all_plots

ggsave("plots/FigS19.png", all_plots, height = 5.2, width = 7.5)

###################################################################
###################################################################
# Test: Compare Rt1 values
cols1 <- c("#08519c", "#440154ff", "#9ecae1")

plotfunc2 <- function(df, y, eff, Rt2_val, ylim, caption) {
  dat <- filter(df, name == y, efficacy == eff, Rt2 == Rt2_val)
  ggplot(data = dat, aes(x = income_group, y = value/50e6*1e3, fill = Rt1)) +
    geom_col(position = "dodge") +
    xlab("Income setting") +
    ylab(paste0(y," per thousand")) +
    labs(x = "Income setting", y = paste0(y," per thousand"), fill = "Rt1", title = caption, parse = T) +
    lims(y = c(0, ylim)) +
    scale_fill_manual(values = cols1) +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.line = element_line(),
          plot.caption = element_text(hjust = 0.5))
}

d2 <- filter(d1, hs_constraints == "Present") %>%
  mutate(Rt1 = factor(Rt1))
g5 <- plotfunc2(d2, "Deaths averted", eff = 70, Rt2_val = 2, ylim = 4, caption = "Efficacy = 70%")
g5
g6 <- plotfunc2(d2, "Deaths averted", eff = 90, Rt2_val = 2, ylim = 4, caption = "Efficacy = 90%")
g6

g6 <- plotfunc2(d1, "Deaths averted", eff = 70, Rt2_val = 2, ylim = 4, caption = bquote("R"[t1]==1.05))
g7 <- plotfunc2(d1, "Deaths averted", eff = 70, Rt2_val = 2, ylim = 4, caption = bquote("R"[t1]==1.1))

all_plots2 <- (g5 | g6 | g7 ) + plot_layout(guide = "collect", ncol = 1, nrow = 3)+ plot_annotation(tag_levels = 'A')

all_plots2
###################################################################
# Compare efficacy


d2 <- filter(d1, hs_constraints == "Present", efficacy == 70) %>%
  mutate(Rt1 = factor(Rt1))

gRt1 <- ggplot(data = d2, aes(x = income_group, y = value/50e6*1e3, fill = Rt1)) +
  geom_col(position = "dodge") +
  xlab("Income setting") +
  ylab(paste0(y," per thousand")) +
  labs(x = "Income setting", y = " Deaths averted per thousand", fill = "Rt1", title = "Vary Rt1", parse = T) +
  scale_fill_manual(values = cols1) +
  theme_bw() +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.caption = element_text(hjust = 0.5))
ggsave("plots/impact_Rt1.png", gRt1, height = 4, width = 5)
