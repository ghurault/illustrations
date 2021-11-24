# Notes -------------------------------------------------------------------

# Illustrate the lpd, Accuracy (probability that predictions within a value d of the true score)
# Also report the CRPS, and interpret the metrics in terms of precision and trueness

# Initialisation ----------------------------------------------------------

library(tidyverse)
library(cowplot)
library(gganimate)

d <- 1 # error margin
y <- 0 # observation set at 0
col <- "#E69F00"

df <- data.frame(mu = c(0, 0, -2, -2),
                 sigma = c(.5, 2, .5, 2))

df <- df %>%
  mutate(ID = 1:n()) %>%
  mutate(Label = case_when(mu >= -1 & sigma <= .75 ~ "High trueness\nHigh precision",
                           mu >= -1 & sigma > .75 ~ "High trueness\nLow precision",
                           mu < -1 & sigma <= .75 ~ "Low trueness\nHigh precision",
                           mu < -1 & sigma > .75 ~ "Low trueness\nLow precision")) %>%
  mutate(Accuracy = pnorm(y + d, mu, sigma) - pnorm(y - d, mu, sigma),
         lpd = dnorm(y, mu, sigma, log = TRUE),
         ID = fct_reorder(factor(ID), lpd, .desc = FALSE),
         Label = fct_reorder(Label, lpd, .desc = FALSE),
         CRPS = scoringRules::crps(y, family = "normal", mean = mu, sd = sigma)) %>%
  arrange(ID)

# Relationship between metrics --------------------------------------------

ggplot(data = df, aes(x = lpd, y = Accuracy, label = Label)) + # Can change x and y
  geom_label() +
  # scale_x_continuous(limits = c(-.25, 1.25)) +
  theme_bw(base_size = 15)

# Heatmap Trueness/Precision ----------------------------------------------
# Assuming prediction is normal(mu, sigma)

# trueness (accuracy): |y - mu|
# precision: sigma (different from variance = inverse precision !)

lbl <- data.frame(x = c(.3, .3, 1.7, 1.7),
                  y = c(0.5, 2.5, 0.5, 2.5),
                  label = c("Accurate\nPrecise", "Accurate\nNot Precise",
                            "Not Accurate\nPrecise", "Not Accurate\nNot Precise"))

hm <- expand.grid(mu = seq(0, 3, .01),
                  sigma = seq(.01, 2, .01)) %>%
  mutate(Accuracy = pnorm(y + d, mu, sigma) - pnorm(y - d, mu, sigma),
         lpd = dnorm(y, mu, sigma, log = TRUE),
         CRPS = scoringRules::crps(y, family = "normal", mean = mu, sd = sigma))

ggplot() +
  geom_raster(data = hm, aes(x = sigma, y = mu, fill = CRPS)) + # can change fill
  scale_fill_viridis_c(direction = -1) + # -1 for CRPS, 1 for Accuracy and lpd
  coord_cartesian(expand = c(0, 0)) +
  geom_label(data = lbl, aes(x = x, y = y, label = label)) +
  theme_classic(base_size = 15)

# Animation ---------------------------------------------------------------

if (TRUE) {
  # Group by x
  # Transition from mode to mode weird

  pa <- expand_grid(df, x = seq(-8, 8, .1)) %>%
    mutate(dx = dnorm(.data$x, mean = .data$mu, sd = .data$sigma),
           # px = pnorm(.data$x, mean = .data$mu, sd = .data$sigma),
           dx_acc = dx * (x >= y - d & x <= dx + d)) %>%
    ggplot() + # aes(group = factor(round(px, 1)))
    # Distribution
    geom_line(aes(x = x, y = dx)) +
    geom_point(x = 0, aes(y = exp(lpd)), size = 1.5) + # cf. lpd
    geom_ribbon(aes(x = x, ymax = dx_acc), ymin = 0, alpha = 0.5, fill = col) +
    # Metrics
    geom_label(x = -6, y = 0.8, size = 5, fontface = "bold", # fill = col,
               aes(label = paste0("Accuracy = ", signif(Accuracy, 2) * 100, "%"))) +
    geom_label(x = -6, y = 0.75, size = 5, fontface = "bold",
               aes(label = paste0("lpd = ", signif(lpd, 2)))) +
    geom_label(x = -6, y = 0.7, size = 5, fontface = "bold",
               aes(label = paste0("CRPS = ", signif(CRPS, 2)))) +
    # Animation
    transition_states(Label, state_length = .5, transition_length = .5) + #
    ease_aes("circular-in-out") +
    #
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(title = "{closest_state}", y = "Density") +
    theme_bw(base_size = 15)

} else {
  # Group by quantiles
  # The distribution moves "better" but the transition of the accuracy ribbon is weird (not anchored to the curve)
  # Also it takes very long (especially with metrics)

  tmp1 <- expand_grid(df, px = seq(0, 1, length.out = 1e4)) %>%
    mutate(x = qnorm(.data$px, mean = .data$mu, sd = .data$sigma),
           dx = dnorm(.data$x, mean = .data$mu, sd = .data$sigma),
           Type = "Curve") %>%
    filter(abs(x) < Inf)
  tmp2 <- expand_grid(df, x = seq(-8, 8, .1)) %>%
    mutate(dx = dnorm(.data$x, mean = .data$mu, sd = .data$sigma),
           px = pnorm(.data$x, mean = .data$mu, sd = .data$sigma),
           dx = dx * (x >= y - d & x <= dx + d),
           Type = "Ribbon")

  pa <- ggplot() +
    # Distribution
    geom_line(data = tmp1, aes(x = x, y = dx)) +
    geom_point(data = tmp2, x = 0, aes(y = exp(lpd)), size = 1.5) + # cf. lpd
    geom_ribbon(data = tmp2, aes(x = x, ymax = dx), ymin = 0, alpha = 0.5, fill = col) +
    # Animation
    transition_states(Label, state_length = .6, transition_length = .4) + #
    ease_aes("circular-in-out") +
    #
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(title = "{closest_state}", y = "Density") +
    theme_bw(base_size = 15)

}

animate(pa,
        width = 13 * 60,
        height = 8 * 60,
        units = "px")

# anim_save(here::here("plots", "prob_metrics.gif"))

# Same as animation but as a grid ---------------------------------------------

tmp <- expand_grid(df, x = seq(-8, 8, .1)) %>%
  mutate(dx = dnorm(.data$x, mean = .data$mu, sd = .data$sigma),
         dx_acc = dx * (x >= y - d & x <= dx + d))

lapply(df[["ID"]],
       function(i) {
         ym <- max(tmp$dx)
         tmp %>%
           filter(ID == i) %>%
           ggplot() +
           # Distribution
           geom_line(aes(x = x, y = dx)) +
           geom_point(x = 0, aes(y = exp(lpd)), size = 2.5) + # cf. lpd
           geom_ribbon(aes(x = x, ymax = dx_acc), ymin = 0, alpha = 0.5, fill = col) +
           # Metrics
           geom_label(x = -6, size = 5, fontface = "bold", # fill = col,
                      aes(y = ym, label = paste0("Accuracy = ", signif(Accuracy, 2) * 100, "%"))) +
           geom_label(x = -6, size = 5, fontface = "bold",
                      aes(y = ym * 0.9, label = paste0("lpd = ", signif(lpd, 2)))) +
           geom_label(x = -6, size = 5, fontface = "bold",
                      aes(y = ym * 0.8, label = paste0("CRPS = ", signif(CRPS, 2)))) +
           #
           geom_vline(xintercept = 0, linetype = "dashed") +
           scale_y_continuous(expand = expansion(mult = c(0, .1)), limits = c(0, ym)) +
           scale_x_continuous(breaks = c(-5, -1, 0, 1, 5), labels = c("-5d", "-d", 0, "d", "+5d")) +
           labs(title = df$Label[df$ID == i], y = "Density", x = "Error") +
           theme_bw(base_size = 15)
       }) %>%
  plot_grid(plotlist = ., ncol = 2)

# ggsave(here::here("plots", "prob_metrics.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 3)
