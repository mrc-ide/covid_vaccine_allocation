library(tidyr)
library(ggplot2)
library(dplyr)

create_age_targets <- function() {
  age_target_all <-
    expand.grid(a = 0:1, b = 0:1, c = 0:1, d = 0:1, e = 0:1, f = 0:1, g = 0:1, h = 0:1, j = 0:1, k = 0:1, l = 0:1, m = 0:1, n = 0:1, p = 0:1, q = 0:1, r = 0:1, s = 0:1)
  
  contig <- apply(age_target_all, 1, function(x) {
    dx1 <- (diff(which(x == 1)))
    dx2 <- (diff(which(x == 0)))
    all(dx2 == 1) | (sum(dx2 > 1) == 1 & sum(dx1 > 1) <= 1)
  })
  
  age_target <- age_target_all[contig, ]
}
t <- create_age_targets()

pd <- t %>%
  mutate(strategy = 1:n(),
         row = rep(1:4, each = 350)[1:nrow(t)]) %>%
  pivot_longer(-c(strategy, row), names_to = "Age group", values_to = "Target") %>%
  mutate(`Age group` = as.numeric(as.factor(`Age group`)))

x <- seq(0,80,5)
y <- seq(4,84,5)
z <- paste0(x,"-",y)
z[17] <- "80+"

pd1 <- filter(pd, Target == 1)

g1 <- ggplot(pd1, aes(x = strategy, y = `Age group`, fill = factor(Target))) + 
  geom_tile() +
  facet_wrap(~ row, scales = "free", ncol = 1) + 
  #scale_x_continuous("strategy") +
  scale_fill_manual(values = c("darkgrey"), labels = c("Vaccinated"), name = "") +
  scale_y_continuous(breaks = 1:17, labels = z) +
  scale_x_continuous( expand = c(0, 0))+
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_line(colour = "grey", size = 0.5),
        panel.border = element_blank(),
        axis.line = element_line(),
        legend.position="bottom") +
  labs(x = "Strategy")

g1

ggsave("plots/FigS4.png", g1, height = 9.5, width = 7.5)

