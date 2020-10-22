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

pd <- t %>%
  mutate(strategy = 1:n(),
         row = rep(1:4, each = 350)[1:nrow(t)]) %>%
  pivot_longer(-c(strategy, row), names_to = "Age group", values_to = "Target") %>%
  mutate(`Age group` = as.numeric(as.factor(`Age group`)))
x <- seq(0,80,5)
y <- seq(4,84,5)
z <- paste0(x,"-",y)
z[17] <- "80+"
g1 <- ggplot(pd, aes(x = strategy, y = `Age group`, fill = factor(Target))) + 
  geom_tile() +
  facet_wrap(~ row, scales = "free", ncol = 1) + 
  scale_fill_manual(values = c("white", "darkgrey"), labels = c("No vaccine", "Vaccine"), name = "") +
  scale_y_continuous(breaks = 1:17, labels = z) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  labs(x = "Strategy")

ggsave("plots/age_band_example.png", g1, height = 8, width = 7.5)

