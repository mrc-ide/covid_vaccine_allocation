##################################################################################
# illustrate impact of longer vaccine roll-out, for varying Rt2

cols3 <- c("#8c2d04", "#fe9929", "#fed976")

pd9 <- readRDS("output/4_gradual_rollout.rds") %>%
  mutate(vaccine_start = vaccine_start + t_start,
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0,
         vaccine_period = factor(vaccine_period)) %>%
  filter(income_group == "HIC",
         R0 == 2.5,
         Rt1 == 1.1,
         vaccine_period == 365,
         efficacy == 0.9) %>%
  select(-output_cf, -output_age, -output_age_cf) %>%
  unnest(output) %>%
  filter(compartment == "deaths")

pd9_cf <- pd9 %>%
  filter(coverage == 0, vaccine_coverage_mat == "All")

pd9 <- filter(pd9, coverage == 0.8)

pd10 <- readRDS("output/4_gradual_rollout.rds") %>%
  mutate(vaccine_start = vaccine_start + t_start,
         Rt1 = (1-reduction1)*R0,
         Rt2 = (1-reduction2)*R0) %>%
  filter(coverage == 0.8,
         income_group == "HIC",
         R0 == 2.5,
         Rt1 == 1.1,
         vaccine_period == 365,
         efficacy == 0.9) %>%
  mutate(deaths_averted = (deaths_averted_2021)/50e6*1000)

##################################################################################
g1_plot_line <- function(dat = dat, dat_cf = dat_cf, Rt2_val = Rt2_val){
  dat <- filter(dat, Rt2 == Rt2_val)
  dat_cf <- filter(dat_cf, Rt2 == Rt2_val)
  ggplot() +
  geom_segment(data = dat, aes(x = vaccine_start-21, y = 0, xend = vaccine_start-21, yend = Inf), col = "black", linetype = "dashed") +
  geom_segment(data = dat, aes(x = vaccine_start+365, y = 0, xend = vaccine_start+365, yend = Inf), col = "black", linetype = "dotted") +
  geom_line(data = filter(dat, t >= vaccine_start), aes(x = t, y = (value/50e6*1e6), col = vaccine_coverage_mat), size = 1) +
  geom_line(data = filter(dat_cf), aes(x = t, y = (value/50e6*1e6)), col = "black", size = 1, linetype = "longdash") +
  labs(x = "Time", y = "Deaths per million per day", col="Vaccine targeting strategy", title = paste0("Rt2 = ", Rt2_val)) +
  theme_bw() +
  scale_x_continuous(breaks = c(183,366,549,732,914), labels = c("Jul '20", "Jan '21", "Jul '21", "Jan '22", "Jul '22")) +
  theme(strip.background = element_rect(colour = NA, fill = NA),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.text.align = 0) +
  scale_color_manual(values = cols3, labels = c("All", "Target older", "Target working-age"))
}

g1_plot_col <- function(dat = dat, Rt2_val = Rt2_val){
  dat <- filter(dat, Rt2 == Rt2_val)
  ggplot(data = dat, aes(x = factor(vaccine_coverage_mat), y = deaths_averted , fill = factor(vaccine_coverage_mat))) +
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
}
##################################################################################

p1 <- g1_plot_line(dat = pd9, dat_cf = pd9_cf, Rt2_val = 1.3)
p2 <- g1_plot_line(dat = pd9, dat_cf = pd9_cf, Rt2_val = 1.5)
p3 <- g1_plot_line(dat = pd9, dat_cf = pd9_cf, Rt2_val = 2.0)
p4 <- g1_plot_col(dat = pd10, Rt2_val = 1.3)
p5 <- g1_plot_col(dat = pd10, Rt2_val = 1.5)
p6 <- g1_plot_col(dat = pd10, Rt2_val = 2.0)

##################################################################################
# combine plots

all_plots <- (p1 | p4 | p2 | p5 | p3 | p6 ) + plot_layout(guide = "collect", ncol = 2, nrow = 3)+ plot_annotation(tag_levels = 'A')

all_plots

ggsave("plots/FigS9.png", all_plots, height = 10, width = 10)

