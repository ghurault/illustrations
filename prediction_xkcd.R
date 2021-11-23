# Notes -------------------------------------------------------------------

# Illustrate time-series prediction, xkcd style

# Need to download xkcd fonts, cf. vignette "xkcd-intro"

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart session)

library(tidyverse)
library(forecast)
library(xkcd)

set.seed(78)

tp <- 60 # Prediction time
h <- 30 # Prediction horizon

# Data --------------------------------------------------------------------

# Smoothed Brownian motion
t <- seq(0, 100, 1)
bm <- 50 + cumsum(rnorm(length(t), mean = 0, sd = 1))
y <- tibble(t, bm) %>%
  loess(bm ~ t, data = ., span = .7) %>%
  predict()
y <- y[1:tp]

# Forecast
f <- ts(y) %>%
  ets() %>%
  forecast(h = 30, level = 75) %>%
  as_tibble()
colnames(f) <- c("mean", "lower", "upper")
f <- mutate(f, t = tp + 1:h)

# Concatenate
df <- tibble(t = 1:tp,
             y = y,
             upper = y,
             lower = y) %>%
  bind_rows(
    f %>%
      mutate(label = "A"),
    f %>%
      mutate(coef = seq(1, .95, length.out = h),
             upper = upper * coef,
             lower = lower * coef,
             label = "B") %>%
      select(-coef)
  )

# XKCD plot --------------------------------------------------------------------

xr <- c(0, 100) # range(df$t)
yr <- c(floor(min(df[["lower"]])), ceiling(max(df[["upper"]])))

p <- ggplot(data = df) +
  geom_line(aes(x = t, y = y), lwd = 1.5, colour = "black") +
  geom_ribbon(aes(x = t, ymin = lower, ymax = upper, fill = label), alpha = .3) +
  scale_fill_manual(values = HuraultMisc::cbbPalette[c(7, 6)]) +
  xkcdaxis(xr, yr) +
  labs(x = "Time", y = "Severity") +
  theme(text = element_text(size = 20),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
p

mapping <- aes(x, y, scale, ratioxy, angleofspine,
               anglerighthumerus, anglelefthumerus,
               anglerightradius, angleleftradius,
               anglerightleg, angleleftleg, angleofneck)

dataman <- data.frame(x = 99, y = 42.5,
                      scale = 1.5,
                      ratioxy = diff(xr) / diff(yr),
                      angleofspine =  -pi / 2 - pi / 24,
                      anglerighthumerus = -7 * pi / 12,
                      anglelefthumerus = -pi / 2 - pi / 4,
                      anglerightradius = -5 * pi / 6,
                      angleleftradius = 2 * pi / 3,
                      angleleftleg = 3 * pi / 2 + pi / 12 ,
                      anglerightleg = 3 * pi / 2 - pi / 12,
                      angleofneck = -0.42 * pi)


p + xkcdman(mapping, dataman) +
  annotate("text", x = 92, y = 42.5, label = "A", family = "xkcd") +
  annotate("text", x = 92, y = 40.5, label = "B", family = "xkcd") +
  annotate("text", x = 89, y = 45.5, label = "A or B ?", family = "xkcd") +
  xkcdline(aes(x = xbegin, y = ybegin, xend = xend, yend = yend),
           data = data.frame(xbegin = 95.5, xend = 90, ybegin = 43.4, yend = 45), xjitteramount = 0.12)

# ggsave(here::here("plots", "prediction_xkcd.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 1.2)
