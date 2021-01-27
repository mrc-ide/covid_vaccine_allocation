### Plotting efficacy/coverage outputs
# Figure 2, Figure S8

### Load packages ##############################################################
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)

### Load function ##############################################################
source("R/functions.R")

### Plotting stuff #############################################################
cols <- RColorBrewer::brewer.pal(12, "Paired")[c(2, 4, 6, 10)]

### Load data ##################################################################
d <- readRDS("output/3_vacc_characteristics.rds") %>%
  mutate(efficacy = factor(efficacy*100),
         income_group = factor(income_group, levels = c("LIC","LMIC", "UMIC", "HIC")),
         mode = factor(mode, levels = c("Infection", "Disease")),
         vaccine_start = vaccine_start + t_start,
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0)

# Theoretical herd immunity threshold for imperfect vaccine ####################
# From  “Herd Immunity”: A Rough Guide (Paul Fine, Ken Eames, David L. Heymann)
R <- c(4, 3.5, 3, 2.5, 2, 1.5)
efficacy <- seq(0.5, 1, 0.001)
pd1 <- expand_grid(efficacy = efficacy, R = R) %>%
  mutate(threshold = (1 - (1 / R)) * (1 / efficacy)) %>%
  filter(threshold <= 1) %>%
  #mutate(label = if_else(R == 2, "Rt2 = 2", "R0 = 2.5"))
  mutate(label = paste0("R[t]==", R))

p1 <- ggplot(pd1, aes(x = efficacy * 100, y = threshold * 100, col = label)) +
  geom_line() +
  ylim(50, 100) +
  ylab("Theoretical coverage for herd immunity (%)") +
  xlab("Efficacy (%)") +
  theme_bw() +
  theme(aspect.ratio = 1,
        strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_viridis_d(option = "C", begin = 0, end = 0.6, name = "", labels = function(label) parse(text=label))
 
p1 
  
# Coverage and efficacy
pd2 <- d %>%
  filter(income_group == "HIC",
         duration_R == 365,
         hs_constraints == "Present")

p2 <- ggplot(pd2, aes(x = coverage * 100, y = deaths_averted_2021 / 50e6 * 1e3, col = efficacy, lty = mode)) + 
  geom_line() +
  scale_colour_viridis_d("Efficacy (%)", end = 0.8, direction = -1) +
  scale_linetype("Mode") +
  xlab("Coverage (%)") +
  ylab("Deaths averted per thousand") +
  theme_bw() +
  theme(aspect.ratio = 0.8,
        strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0)

# Combine subplots
cov_efficacy_plot <- (p1 | p2) + plot_annotation(tag_levels = 'A')

cov_efficacy_plot

ggsave("plots/Fig2.png", cov_efficacy_plot, height = 6, width = 12)

# calculate % deaths averted
prop_averted <- pd2 %>%
  filter(efficacy == 90,
         coverage >= 0.7)
##########################################################

# Repeat the coverage/efficacy/mode plot for all income settings

# Coverage and efficacy
pd3 <- d %>%
  filter(duration_R == 365)
pd3[which(pd3$hs_constraints == "Absent"),]$hs_constraints <- "HS Constraints: Absent"
pd3[which(pd3$hs_constraints == "Present"),]$hs_constraints <- "HS Constraints: Present"

p3 <- ggplot(pd3, aes(x = coverage * 100, y = deaths_averted_2021 / 50e6 * 1e3, col = efficacy, lty = mode)) + 
  geom_line() +
  scale_colour_viridis_d("Efficacy (%)", end = 0.8, direction = -1) +
  scale_linetype("Mode") +
  xlab("Coverage (%)") +
  ylab("Deaths averted per thousand") +
  theme_bw() +
  facet_wrap(hs_constraints~income_group, nrow = 2) +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

p3

ggsave("plots/FigS7.png", p3, height = 6, width = 10)
