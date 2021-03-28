# Figure 3

# Load packages
library(dplyr)
library(ggplot2)
library(patchwork)
source("R/functions.R")

# Plotting stuff
Rt1_labs <- c('R[t1]=0.9', 'R[t1]=1', 'R[t1]=1.1')
names(Rt1_labs) <- c(0.9, 1, 1.1)

R0_labs <- c('R[0]=2.5', 'R[0]=3.0')
names(R0_labs) <- c(2.5, 3.0)

cols1 <- c("#08519c", "#440154ff", "#9ecae1")
cols2 <- c( "#993366","#440154ff", "#e6b3cc")
cols3 <- c("#8c2d04", "#fe9929", "#fed976")

lightgreen <- "#bbdf27"
teal <- "#25848e"
darkpurple <- "#440154"

# Load outputs #################################################################
df_main <- readRDS("output/4_epidemic_characteristics.rds") %>%
  filter(hs_constraints == "Present") %>%
  mutate(vaccine_start = vaccine_start + t_start,
         timing1 = timing1 + t_start,
         timing2 = timing2 + t_start,
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0) %>%
  mutate(prop_R = map2_dbl(output, vaccine_start, summarise_R, pop = 50e6))%>%
  mutate(R0_labs = paste0("R[0]==", R0),
         Rt1_labs = paste0("R[t1]==", Rt1),
         Rt2_labs = paste0("R[t2]==", Rt2)) %>%
  mutate(R_labs = paste0(R0_labs, "*\", \"*~", Rt1_labs, "*\", \"*~", Rt2_labs, "*\", \"*~", "Pr[R]==", round(prop_R,2)*100, "*\"%\""))

df_counter <- df_main %>%
  filter(coverage == 0) %>%
  unnest(cols = output_cf) %>%
  filter(compartment == "deaths",
         t < 1035) %>%
  mutate(Rt1 = factor(Rt1),
         Rt2 = factor(Rt2),
         duration_R = factor(duration_R),
         duration_V = factor(duration_V)
  ) %>%
  group_by(R0, Rt1, duration_V) %>%
  mutate(height = max(value, na.rm = TRUE)*0.9)

#################################################################
# Plot trajectories assuming 1y immunity, HIC income setting

pd1 <- df_counter %>% 
  filter((duration_R == 365 & duration_V == 5000),
         income_group == "HIC") %>%
  mutate(R0 = factor(R0),
         Rt1 = factor(Rt1),
         Rt2 = factor(Rt2)) %>%
  filter((R0 == 2.5 & Rt1 == 0.9 & Rt2 == 2.0) | (R0 == 2.5 & Rt1 == 1.1 & Rt2 == 2) | (R0 == 3.0 & Rt1 == 0.9 & Rt2 == 2))

g1 <- ggplot() +
  geom_segment(data = pd1, aes(x = vaccine_start-21, y = 0, xend = vaccine_start-21, yend = Inf, linetype = "Vaccination start"), col = "black") +
  geom_segment(data = pd1, aes(x = vaccine_start+30, y = 0, xend = vaccine_start+30, yend = Inf, linetype = "Vaccination stop"), col = "black") +
  geom_line(data = pd1, aes(x = t, y = (value/50e6*1e6), col = R_labs), size= 1) +
  labs(x = "Time", y = "Deaths per million per day", col="A,B: Varying proportion in R at vaccination", linetype = "") +
  theme_bw() +
  scale_x_continuous(breaks = c(183,366,549,732,914), labels = c("Jul '20", "Jan '21", "Jul '21", "Jan '22", "Jul '22")) +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = cols1, labels = function(R_labs) parse(text=R_labs)) +
  scale_linetype_manual(values = c("dashed", "dotted"))

g1

##################################################################################
pd2 <- df_main %>%
  filter(coverage == 0.8) %>%
  filter((duration_R == 365 & duration_V == 5000), income_group == "HIC") %>%
  mutate(R0 = factor(R0),
         Rt1 = factor(Rt1),
         Rt2 = factor(Rt2)) %>%
  filter((R0 == 2.5 & Rt1 == 0.9 & Rt2 == 2.0) | (R0 == 2.5 & Rt1 == 1.1 & Rt2 == 2) | (R0 == 3.0 & Rt1 == 0.9 & Rt2 == 2)) %>%
  select(R_labs, deaths_averted_2021, deaths_averted_2022) %>%
  mutate(deaths_averted = (deaths_averted_2021 + deaths_averted_2022)/50e6*1000)

g2 <- ggplot(data = pd2, aes(x = R_labs, y = deaths_averted , fill = R_labs)) +
  geom_col(alpha = 0.8) +
  theme_bw() +
  scale_fill_manual(values = cols1, labels = function(R_labs) parse(text=R_labs)) + 
  scale_x_discrete(labels = c("4%", "11%", "14%")) +
  labs(x = "Proportion in R at vaccination", y = "Deaths averted per thousand", fill = "Proportion in R at \nvaccination (A, B)") +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none")

g2

##################################################################################
pd3 <- df_counter %>% 
  filter((duration_R == 365 & duration_V == 5000),
         income_group == "HIC") %>%
  mutate(R0 = factor(R0),
         Rt1 = factor(Rt1),
         Rt2 = factor(Rt2)) %>%
  filter((R0 == 2.5 & Rt1 == 1.1 & Rt2 == 1.5) | (R0 == 2.5 & Rt1 == 1.1 & Rt2 == 2.0) | (R0 == 2.5 & Rt1 == 1.1 & Rt2 == 2.5))

pd3a <- filter(pd3, t < vaccine_start + 30)
pd3b <- filter(pd3, t >= vaccine_start + 30)

g3 <-ggplot() +
  geom_segment(data = pd3a, aes(x = vaccine_start-21, y = 0, xend = vaccine_start-21, yend = Inf, linetype = "Vaccination start"), col = "black") +
  geom_segment(data = pd3a, aes(x = vaccine_start+30, y = 0, xend = vaccine_start+30, yend = Inf, linetype = "Vaccination stop"), col = "black") +
  geom_line(data = pd3a, aes(x = t, y = (value/50e6*1e6)), col = "black", size = 1) +
  geom_line(data = pd3b, aes(x = t, y = (value/50e6*1e6), col = R_labs), size = 1) +
  labs(x = "Time", y = "Deaths per million per day", col="C,D: Transmission from March 2021", linetype = "") +
  theme_bw() +
  scale_x_continuous(breaks = c(183,366,549,732,914), labels = c("Jul '20", "Jan '21", "Jul '21", "Jan '22", "Jul '22")) +
  theme(strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  #scale_color_viridis_d(option = "D", begin = 0, end = 0.9, direction = -1, labels = function(R_labs) parse(text=R_labs)) +
 scale_color_manual(values = c(lightgreen, darkpurple, teal), labels = function(R_labs) parse(text=R_labs)) +
  scale_linetype_manual(values = c("dashed", "dotted"))

g3

##################################################################################
pd4 <- df_main %>%
  filter(coverage == 0.8,
         (duration_R == 365 & duration_V == 5000),
         income_group == "HIC") %>%
  mutate(R0 = factor(R0),
         Rt1 = factor(Rt1),
         Rt2 = factor(Rt2)) %>%
  filter((R0 == 2.5 & Rt1 == 1.1 & Rt2 == 1.5) | (R0 == 2.5 & Rt1 == 1.1 & Rt2 == 2) | (R0 == 2.5 & Rt1 == 1.1 & Rt2 == 2.5)) %>%
  select(R_labs, deaths_averted_2021, deaths_averted_2022) %>%
  mutate(deaths_averted = (deaths_averted_2021 + deaths_averted_2022)/50e6*1000)

g4 <- ggplot(data = pd4, aes(x = R_labs, y = deaths_averted , fill = R_labs)) +
  geom_col(alpha = 0.8) +
  theme_bw() +
  scale_fill_manual(values = c(lightgreen, darkpurple, teal), labels = function(R_labs) parse(text=R_labs)) +
  scale_x_discrete(labels = c("1.5", "2.0", "2.5")) +
  labs(x = expression(paste("R"[t2])), y = "Deaths averted per thousand", fill = "", parse = T) +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none")

g4

##################################################################################
# illustrate impact of longer vaccine roll-out
pd9 <- readRDS("output/4_gradual_rollout.rds") %>%
  mutate(vaccine_start = vaccine_start + t_start,
        Rt1 = (1-reduction1)*R0,
        Rt2 = (1-reduction2)*R0,
        vaccine_period = factor(vaccine_period)) %>%
  filter(income_group == "HIC",
         R0 == 2.5,
         Rt1 == 1.1,
         Rt2 == 2,
         efficacy == 0.9) %>%
  select(-output_cf, -output_age, -output_age_cf) %>%
  unnest(output) %>%
  filter(compartment == "deaths",
         vaccine_period == 365)

pd9_cf <- pd9 %>%
  filter(coverage == 0, vaccine_coverage_mat == "All")

pd9 <- filter(pd9, coverage == 0.8)

g9 <- ggplot() +
  geom_segment(data = pd9, aes(x = vaccine_start-21, y = 0, xend = vaccine_start-21, yend = Inf), col = "black", linetype = "dashed") +
  geom_segment(data = pd9, aes(x = vaccine_start+365, y = 0, xend = vaccine_start+365, yend = Inf), col = "black", linetype = "dotted") +
  geom_line(data = filter(pd9, t >= vaccine_start), aes(x = t, y = (value/50e6*1e6), col = vaccine_coverage_mat), size = 1) +
  geom_line(data = filter(pd9_cf), aes(x = t, y = (value/50e6*1e6)), linetype = "longdash", col = "black", size = 1) +
  labs(x = "Time", y = "Deaths per million per day", col="E,F: Vaccine targeting strategy") +
  theme_bw() +
  scale_x_continuous(breaks = c(183,366,549,732,914), labels = c("Jul '20", "Jan '21", "Jul '21", "Jan '22", "Jul '22")) +
  theme(strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = cols3, labels = c("All", "Target older", "Target working-age"))

g9

pd10 <- readRDS("output/4_gradual_rollout.rds") %>%
  mutate(vaccine_start = vaccine_start + t_start,
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0) %>%
  filter(coverage == 0.8,
         income_group == "HIC",
         R0 == 2.5,
         Rt1 == 1.1,
         Rt2 == 2,
         vaccine_period == 365,
         efficacy == 0.9) %>%
  mutate(deaths_averted = (deaths_averted_2021 + deaths_averted_2022)/50e6*1000)


g10 <- ggplot(data = pd10, aes(x = factor(vaccine_coverage_mat), y = deaths_averted , fill = factor(vaccine_coverage_mat))) +
  geom_col(alpha = 0.8) +
  theme_bw() +
  scale_fill_manual(values = c(cols3)) +
  scale_x_discrete(labels = c("All", "Target older", "Target\nworking-age")) +
  labs(x = "Vaccine targeting strategy", y = "Deaths averted per thousand", fill = "", parse = T) +
  theme(strip.background = element_rect(fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0,
        legend.position = "none")

g10

##################################################################################
# 90% vaccine efficacy, 80% coverage, 4 income settings
# show life years saved and deaths averted as bar chart

# plotting function
plotfunc <- function(df, y) {
  ggplot(filter(df, name == y), aes(x = income_group, y = value/50e6*1e3, fill = hs_constraints)) +
    geom_col(position = "dodge") +
    xlab("Income setting") +
    ylab(paste0(y," per thousand")) +
    scale_fill_manual(values = c("grey25", "grey70"), name = "G,H: Health system constraints") +
    theme_bw() +
    theme(strip.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.line = element_line())
}

# Load data
d5 <- readRDS("output/3_vacc_characteristics.rds") %>%
  mutate(efficacy = factor(efficacy*100),
         income_group = factor(income_group, levels = c("LIC","LMIC", "UMIC", "HIC")),
         vaccine_start = vaccine_start + t_start,
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0) 

# Plot bar chart of 2021 impact
pd5 <- d5 %>%
  filter(efficacy == 90,
         coverage == 0.8,
         duration_R == 365,
         mode == "Combined") %>%
  select(income_group, hs_constraints, deaths_averted_2021, years_life_saved_2021) %>%
  rename("Deaths averted" = "deaths_averted_2021", "Life-years gained" = "years_life_saved_2021") %>%
  pivot_longer(c("Deaths averted", "Life-years gained"))

g5 <- plotfunc(pd5, "Deaths averted")
g6 <- plotfunc(pd5, "Life-years gained")

g5
g6

pd5$value_per_1000 <- pd5$value/50e6*1e3

write_csv(pd5, "averted_per_pop_summary.csv")

##################################################################################
# combine plots

all_plots <- (g1 | g2 | g3 | g4 | g9 | g10 | g5 | g6) + plot_layout(guide = "collect", ncol = 2, nrow = 5)+ plot_annotation(tag_levels = 'A')

all_plots

#ggsave("plots/Fig3.png", all_plots, height = 12, width = 10)
ggsave("plots/Fig3.png", all_plots, height = 11, width = 10)

