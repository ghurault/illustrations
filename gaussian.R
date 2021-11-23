# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

library(tidyverse)
library(HuraultMisc)

# Plot --------------------------------------------------------------------

tibble(x = seq(-4, 4, .1),
       density = dnorm(x)) %>%
  ggplot(aes(x = x, y = density)) +
  geom_line(size = 1) +
  geom_area(fill = HuraultMisc::cbbPalette[3]) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(x = "", y = "Density") +
  theme_classic(base_size = 15) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())

ggsave(here::here("plots", "distribution.jpg"), width = 13, height = 8, units = "cm", dpi = 300)
