# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart session)

library(tidyverse)
library(xkcd)

# Thinking man ------------------------------------------------------------

mapping <- aes(x, y, scale, ratioxy, angleofspine,
               anglerighthumerus, anglelefthumerus,
               anglerightradius, angleleftradius,
               anglerightleg, angleleftleg, angleofneck)

dataman <- tibble(x = 0,
                  y = 0,
                  scale = 1 ,
                  ratioxy = 1,

                  angleofspine =  -pi/2,

                  anglelefthumerus = -5 * pi / 6,
                  angleleftradius = -pi / 6,

                  anglerighthumerus = -pi / 6,
                  anglerightradius = 7 * pi / 12,

                  anglerightleg = pi * (3 / 2 - 1 / 12),
                  angleleftleg = pi * (3 / 2 + 1 / 12),
                  angleofneck = -pi / 2 - pi / 12)


ggplot() +
  xkcdman(mapping, dataman) +
  theme_classic(base_size = 15) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank())

# ggsave(file.path("plots", "stickman.png"), width = 5, height = 10, units = "cm", dpi = 300)
