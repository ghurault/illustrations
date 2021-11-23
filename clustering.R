# Initialisation ----------------------------------------------------------

rm(list=ls()) # Clear Workspace (better to restart session)

library(tidyverse)
library(ggalt)
library(cowplot)

set.seed(489632)

N <- 100

# Plot --------------------------------------------------------------------

df <- bind_rows(data.frame(x = rnorm(N, mean = -1, sd = 0.5),
                           y=rnorm(N, mean = -1, sd = 0.5),
                           Cluster = "A"),
                data.frame(x = rnorm(N ,mean = 2, sd = 1),
                           y = rnorm(N, mean = 2, sd = 1),
                           Cluster = "B"))

gg_layout <- function(gp) {
  list(
    geom_point(),
    labs(x = "Feature 1", y = "Feature 2"),
    xlim(c(-3, 5)),
    ylim(c(-3, 5)),
    theme_classic(base_size = 15),
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.text = element_blank())
  )
}

plot_grid(
  ggplot(data = df, aes(x = x, y = y)) +
    gg_layout(),
  ggplot(data = df, aes(x = x, y = y, colour = Cluster)) +
    geom_encircle(lwd = 2) +
    gg_layout(),
  ncol = 2
)

ggsave(here::here("plots", "clustering.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 2)
