# Figure S7

# Load packages
library(dplyr)
library(ggplot2)
library(patchwork)
source("R/functions.R")

################################################################################
# Plotting stuff

vaccine_start_labs <- c("Jan '21", "Feb '21", "Mar '21", "Apr '21", "May '21", "Jun '21", "Jul '21", "Aug '21", "Sep '21", "Oct '21", "Nov '21", "Dec '21")
names(vaccine_start_labs) <- round(seq(366,(365*2),30.5))

duration_R_labs <- c("Long natural immunity", "1y natural immunity", "6m natural immunity")
names(duration_R_labs) <- c(Inf, 365, 183)

duration_V_labs <- c("Long vaccine immunity", "1y vaccine immunity", "6m vaccine immunity")
names(duration_V_labs) <- c(5000, 365, 183)

Rt2_labs <- c("Rt2 = 1.3", "Rt2 = 1.5", "Rt2 = 2")
names(Rt2_labs) <- c(1.3, 1.5, 2)

breaks1 <- c(183,366,549,732,914)
labels1 <- c("Jul '20", "Jan '21", "Jul '21", "Jan '22", "Jul '22")

################################################################################
# Load outputs
timing_out <- readRDS("output/2_trajectories.rds") %>%
  mutate(vaccine_start = vaccine_start + t_start - 21,
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0) %>%
  filter(R0 == 2.5,
         coverage == 0.8,
         Rt2 == 2, 
         income_group == "HIC") %>%
  select(R0, vaccine_start, coverage, Rt1, Rt2, duration_R, duration_V, output, output_cf) %>%
  mutate(Rt1 = factor(Rt1),
         Rt2 = factor(Rt2),
         duration_R = factor(duration_R, levels = c(Inf, 365, 183)),
         duration_V = factor(duration_V, levels = c(5000, 365, 183))) 

################################################################################

# plot trajectories for different combinations of vaccine and natural immunity

pd1 <- timing_out %>%
  select(R0, vaccine_start, coverage, Rt1, Rt2, duration_R, duration_V, output) %>%
  unnest(cols = output) %>%
  filter(compartment == "deaths",
         t < 1035) %>%
  rename("Vaccine" = "value")

pd1_cf <- timing_out %>%
  select(R0, vaccine_start, Rt1, Rt2, duration_R, duration_V, output_cf) %>%
  unnest(cols = output_cf) %>%
  filter(compartment == "deaths",
         Rt2 == 2) %>%
  group_by(R0, Rt1, Rt2, duration_V, duration_R) %>%
  mutate(height = max(value, na.rm = TRUE)*0.9) %>%
  rename("No vaccine" = "value") %>%
  ungroup %>%
  filter(t < 1035)

pd_join <- left_join(pd1, pd1_cf) %>%
  pivot_longer(c("No vaccine", "Vaccine"), names_to = "intervention", values_to = "value")

################################################################################

# plot trajectories for different combinations of vaccine and natural immunity -  where the two durations of immunity are the same (3 subplots)

pd2 <- pd_join %>%
  filter((duration_R == 365 & duration_V == 5000) | (duration_R == 365 & duration_V == 365) | (duration_R == 183 & duration_V == 183))

p2 <- ggplot() + 
  geom_segment(data = pd2, aes(x = vaccine_start, y = 0, xend = vaccine_start, yend = Inf, col = "Vaccination start")) +
  geom_segment(data = pd2, aes(x = vaccine_start + 30 + 21, y = 0,  xend = vaccine_start + 30 + 21, yend = Inf, col = "Vaccination stop")) +
  geom_line(data = pd2, aes(x = t, y = (value/50e6*1e6), linetype = intervention)) +
  geom_line(data = filter(pd2, t < 366), aes(x = t, y = (value/50e6*1e6)), col = "black", linetype = "dashed") +
  ylab("Deaths per million per day") + 
  xlab("Time") + 
  theme_bw() +
  facet_wrap(duration_R~duration_V, labeller = labeller(duration_R = duration_R_labs, duration_V = duration_V_labs), nrow = 1, scales = "free") +
  scale_x_continuous(breaks = breaks1, labels = labels1) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  labs(linetype = "", colour = "") +
  theme(strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_y_continuous(limits = c(0,100))

p2

ggsave("plots/FigS7.png", p2, height = 4, width = 11)


