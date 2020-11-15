
plot1 <- function(dat, caption, ylabs){
  ggplot(dat) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = name2, ymax = name2 + 1, fill = value)) +
    labs(x = "Supply (%)", y = "Age group", fill = "", title = caption) +
    scale_fill_manual(values = c("white", "#440154ff"), labels = c("No vaccine", "Vaccine")) +
    scale_y_continuous(breaks = c(1:17), labels = ylabs) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = NA),
          panel.border = element_blank(),
          axis.line = element_line(),
          legend.text.align = 0,
          plot.caption = element_text(hjust = 0.5))
}

plot2 <- function(dat){
  ggplot() +
    geom_line(data = filter(dat, strategy == "Optimal"), aes(x = vaccine_n_2021/pop_2019*100, y = deaths_averted_2021/pop_2019*1e3, col = strategy), size = 0.8) +
    geom_point(data = filter(dat, strategy != "Optimal"), aes(x = vaccine_n_2021/pop_2019*100, y = deaths_averted_2021/pop_2019*1e3, col = strategy)) +
    labs(x = "Supply (%)", y = "Deaths averted per thousand", col = "Strategy", linetype = "Strategy") +
    theme_bw() +
    lims(y = c(0,5.5)) +
    theme(#aspect.ratio = 1,
      strip.background = element_rect(fill = NA),
      panel.border = element_blank(),
      axis.line = element_line(),
      legend.text.align = 0) +
    scale_color_viridis_d(option = "D", begin = 0, end = 0.85)
}

plot3 <- function(dat){
  ggplot() +
    geom_line(data = filter(dat, strategy == "Optimal"), aes(x = vaccine_n_2021/pop_2019*100, y = additional_life_years_2021/pop_2019*1e3, col = strategy), size = 0.8) +
    geom_point(data = filter(dat, strategy != "Optimal"), aes(x = vaccine_n_2021/pop_2019*100, y = additional_life_years_2021/pop_2019*1e3, col = strategy)) +
    labs(x = "Supply (%)", y = "Life-years gained per thousand", col = "Strategy", linetype = "Strategy") +
    theme_bw() +
    lims(y = c(0,130)) +
    theme(#aspect.ratio = 1,
      strip.background = element_rect(fill = NA),
      panel.border = element_blank(),
      axis.line = element_line(),
      legend.text.align = 0) +
    scale_color_viridis_d(option = "D", begin = 0, end = 0.85)
}

get_ageplot_df <- function(x, icg) {
  pd1 <- x %>%
    filter(strategy == "Optimal", income_group == icg) %>%
    arrange(relative_constraint)
  
  pd1$xmin = c(0, pd1$relative_constraint[-length(pd1$relative_constraint)])
  pd1$xmax = pd1$relative_constraint
  
  pd1 <- pd1 %>%
    separate(age_target, z, "_") %>%
    select(income_group, contains("-"), contains ("+"), relative_constraint, strategy, xmin, xmax) %>%
    pivot_longer(-c(income_group, relative_constraint, strategy, xmin, xmax)) %>%
    mutate(name = factor(name, levels = z),
           name2 = as.numeric(name))
  return(pd1)
}
