# Plot the counterfactual (baseline) epidemic trajectories
# Figure S5, Figure S6, Figure 1

### Load packages ##############################################################
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(tidyr)

################################################################################
# Plotting stuff
Rt1_labs <- c('R[t1]=0.7', 'R[t1]=0.9', 'R[t1]=1.1')
names(Rt1_labs) <- c(0.7, 0.9, 1.1)

R0_labs <- c('R[0]=2.5', 'R[0]=3.0')
names(R0_labs) <- c(2.5, 3.0)

# Load outputs
counter_out <- readRDS("output/1_counter.rds") %>%
  mutate(vaccine_start = vaccine_start + t_start,
         timing1 = timing1 + t_start,
         timing2 = timing2 + t_start,
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0) %>%
  mutate(prop_R = map2_dbl(output, vaccine_start, summarise_R, pop = 50e6))

pd <- counter_out %>%
  filter(coverage == 0) %>%
  select(R0, Rt1, Rt2, income_group, duration_R, output_cf, prop_R, timing1, timing2) %>%
  unnest(cols = output_cf) %>%
  filter(compartment == "deaths",
         t < 1035) %>%
  mutate(Rt1 = factor(Rt1),
         Rt2 = factor(Rt2),
         duration_R = factor(duration_R)
  ) %>%
  group_by(R0, Rt1) %>%
  mutate(height = max(value, na.rm = TRUE)*0.9)

#################################################################
# Plot trajectories for varying levels of transmission, HIC income setting (Figure S6)

pd1 <- pd %>% 
  filter((duration_R == 365),
         income_group == "HIC") %>%
  mutate(R0_labs = paste0("R[0]==", R0),
         Rt1_labs = paste0("R[t1]==", Rt1))

pd1a <- filter(pd1, Rt2 == 2, t<366, is.na(value) == F)
pd1b <- filter(pd1, t >= 366, t<1035, is.na(value) == F)

g1 <- ggplot() +
  geom_line(data = pd1a, aes(x = t, y = (value/50e6*1e6)), col = "black") +
  geom_line(data = pd1b, aes(x = t, y = (value/50e6*1e6), col = Rt2)) +
  labs(x = "Time", y = "Deaths per million per day", color=expression(paste("R"[t2]))) +
  theme_bw() +
  facet_wrap(R0_labs~Rt1_labs, labeller = labeller(.cols = label_parsed, .multi_line = FALSE), nrow = 2) +
  scale_x_continuous(breaks = c(183,366,549,732,914), labels = c("Jul '20", "Jan '21", "Jul '21", "Jan '22", "Jul '22")) +
  scale_color_viridis_d(option = "D", begin = 0, end = 0.9, direction = -1) +
  geom_text(data=pd1,
            aes(x=100,
                y=70,
                label=paste0("Proportion in\nR = ", paste0(round(prop_R,2)*100, "%"))),
            color="darkgray", 
            size=3, hjust = 0) +
  theme(strip.background = element_rect(fill = NA, color = "white"),
        panel.border = element_blank(),
        axis.line = element_line())

g1

ggsave("plots/FigS6.png", g1, height = 4.5, width = 8.5)

#################################################################
# Single trajectory plot, with waning natural immunity

#duration_R_labs <- c("1 year immunity", "Long immunity")
duration_R_labs <- c("", "")
names(duration_R_labs) <- c(365, Inf)

pd3 <- pd %>% 
  filter(R0 == 2.5,
         Rt1 == 1.1,
         income_group == "HIC",
         duration_R == 365)

pd3a <- filter(pd3, Rt2 == 2, t<366, is.na(value) == F)
pd3b <- filter(pd3, t >= 366, t<1035, is.na(value) == F)

plotfunc_counterfactual <- function(pd3, pd3a, pd3b, dur_R){
  g3 <- ggplot() +
    geom_segment(data = filter(pd3, duration_R == dur_R), aes(x = timing1, y = 0, xend = timing1, yend = Inf), col = "black", linetype = "dashed") +
    geom_segment(data = filter(pd3, duration_R == dur_R), aes(x = timing2, y = 0, xend = timing2, yend = Inf), col = "black", linetype = "dashed") +
    geom_line(data = filter(pd3a, duration_R == dur_R), aes(x = t, y = (value/50e6*1e6)), col = "black") +
    geom_line(data = filter(pd3b, duration_R == dur_R), aes(x = t, y = (value/50e6*1e6), col = Rt2)) +
    labs(x = "Time", y = "Deaths per million per day", color=expression(paste("R"[t2]))) +
    theme_bw() +
    scale_x_continuous(breaks = c(183,366,549,732,914), labels = c("Jul '20", "Jan '21", "Jul '21", "Jan '22", "Jul '22")) +
    scale_y_continuous(limits = c(0,70)) +
    # geom_text(data=filter(pd3, duration_R == dur_R),
    #           aes(x=600,
    #               y=65,
    #               label=paste0("proportion in\nR = ", paste0(round(prop_R,2)*100, "%"))),
    #           color="darkgray",
    #           size=3, hjust = 0) +
    annotate("text", x = 90 + 80, y = 65, label = "t[1]", hjust = 0, parse = T) +
    annotate("text", x = 300, y = 55, label = "t[2]", hjust = 0, parse = T) +
    theme(strip.background = element_rect(colour = NA, fill = NA),
          panel.border = element_blank(),
          axis.line = element_line()) +
    scale_color_viridis_d(option = "D", begin = 0, end = 0.9, direction = -1)
}

g3a <- plotfunc_counterfactual(pd3, pd3a, pd3b, dur_R = 365)

ggsave("plots/Fig1.png", g3a, height = 4, width = 4.5)

#################################################################
# Single trajectory plots with and without waning natural immunity, for all income settings

duration_R_labs <- c("1 year immunity", "Long immunity")
names(duration_R_labs) <- c(365, Inf)
pd4 <- pd %>% 
  mutate(duration_R = factor(duration_R, levels = c(365, Inf)),
         income_group = factor(income_group, levels = c("HIC","UMIC", "LMIC", "LIC"))) %>%
  filter(R0 == 2.5,
         Rt1 == 1.1)


pd4a <- filter(pd4, Rt2 == 2, t<366, is.na(value) == F)
pd4b <- filter(pd4, t >= 366, t<1035, is.na(value) == F)

g4 <- ggplot() +
  geom_segment(data = pd4, aes(x = timing1, y = 0, xend = timing1, yend = Inf), col = "black", linetype = "dashed") +
  geom_segment(data = pd4, aes(x = timing2, y = 0, xend = timing2, yend = Inf), col = "black", linetype = "dashed") +
  geom_line(data = pd4a, aes(x = t, y = (value/50e6*1e6)), col = "black") +
  geom_line(data = pd4b, aes(x = t, y = (value/50e6*1e6), col = Rt2)) +
  labs(x = "Time", y = "Deaths per million per day", color=expression(paste("R"[t2]))) +
  theme_bw() +
  facet_wrap(income_group~duration_R, labeller = labeller(duration_R = duration_R_labs, .multi_line = F), nrow = 4) +
  scale_x_continuous(breaks = c(183,366,549,732,914), labels = c("Jul '20", "Jan '21", "Jul '21", "Jan '22", "Jul '22")) +
  geom_text(data=pd4,
            aes(x=650,
                y=65,
                label=paste0("proportion in\nR = ", paste0(round(prop_R,2)*100, "%"))),
            color="darkgray", 
            size=3, hjust = 0) +
  annotate("text", x = 140, y = 75, label = "t[1]", hjust = 0, parse = T) +
  annotate("text", x = 345, y = 65, label = "t[2]", hjust = 0, parse = T)+
  theme(strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_blank(),
        axis.line = element_line()) +
  scale_color_viridis_d(option = "D", begin = 0, end = 0.9, direction = -1)

g4

ggsave("plots/FigS5.png", g4, height = 9, width = 6)
