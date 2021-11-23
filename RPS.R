# Notes -------------------------------------------------------------------

# Illustration of the RPS in the case of forecasts for a discrete "Severity" score, ranging from 0 to 10.
# The forecast follow a (truncated between 0 and 10) Gaussian distribution,
# which is discretised to the nearest integer for RPS calculation (even though the plot show a continuous function...).

# The RPS is the mean square error between the cumulative outcome and cumulative forecast distribution (shaded are square).
# The Ranked Probability Skill Score (RPSS) compares the RPS to a reference RPS (RPS0), `RPSS = 1 - RPS / RPS0`.
# It can be interpreted as a normalised distance to a reference forecast:
# RPSS = 0 means that the forecasts are not better than the reference and RPSS = 1 corresponds to perfect forecasts.
# Here RPS0 corresponds to expected RPS for uniformly distributed outcome and uniform forecast = (k+1)/(6*k)
# where k is the number of categories.

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

library(tidyverse)

# Plot --------------------------------------------------------------------

illustrate_RPS <- function(mu = 5, sigma = 1, observed = 6) {

  stopifnot(between(observed, 0, 10))

  if ((mu / sigma) < -2 || (mu - 10) / sigma > 2) {
    stop("If the distribution mass is too much outside [0, 10], there will be numerical errors")
  }

  lwidth <- 3 # linewidth
  x <- seq(0, 10, .01)

  # RPS calculation
  Z <- stats::pnorm(10, mu, sigma) - stats::pnorm(0, mu, sigma) # Normalisation constant (cf. truncation)
  cumForecast <- (stats::pnorm(pmin(0:10 + .5, 10), mu, sigma) -  stats::pnorm(pmax(0:10 - .5, 0), mu, sigma)) / Z # Discretise Gaussian
  cumForecast <- cumsum(cumForecast)
  cumOutcome <- rep(0, 11)
  cumOutcome[observed + 1] <- 1
  cumOutcome <- cumsum(cumOutcome)
  RPS <- sum((cumForecast - cumOutcome)^2) / (11 - 1)
  RPS0 <- 2 / 11 # expected RPS for uniformly distributed outcome and uniform forecast: (k+1)/(6*k) where k is the number of categories
  RPSS <- 1 - RPS / RPS0

  # PDF
  df1 <- data.frame(Severity = x, Density = stats::dnorm(x, mean = mu, sd = sigma) / Z)
  pdf <- ggplot(data = df1, aes_string(x = "Severity")) +
    geom_line(aes_string(y = "Density"), lwd = lwidth) +
    geom_vline(xintercept = observed, colour = HuraultMisc::cbbPalette[2], lwd = lwidth) +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(df1$Density) * 1.05)) +
    scale_x_continuous(breaks = 0:10) +
    scale_colour_manual(values = HuraultMisc::cbbPalette) +
    theme_classic(base_size = 15)

  # CDF
  df2 <- data.frame(x,
                    Forecast = stats::pnorm(x, mean = mu, sd = sigma) / Z,
                    Outcome = as.numeric(x > observed)) %>%
    mutate(Lower = pmin(.data$Forecast, .data$Outcome),
           Upper = pmax(.data$Forecast, .data$Outcome),
           Fill = "|Error|")
  cdf <- ggplot()+
    geom_line(data = df2 %>%
                select(x, .data$Forecast, .data$Outcome) %>%
                pivot_longer(-x, names_to = "Distribution", values_to = "CD"),
              aes_string(x = "x", y = "CD", colour = "Distribution"), lwd = lwidth) +
    geom_ribbon(data = df2, aes_string(x = "x", ymin = "Lower", ymax = "Upper", fill = "Fill"), alpha = .3) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = 0:10) +
    scale_colour_manual(values = HuraultMisc::cbbPalette) +
    scale_fill_manual(values = "grey12") +
    labs(x = "Severity", y = "Cumulative Density") +
    theme_classic(base_size = 15) +
    theme(legend.title = element_blank(), legend.position = "top")

  cowplot::plot_grid(pdf + labs(subtitle = paste("RPS = ", signif(RPS, 2), " ; RPSS = ", signif(RPSS, 3), sep = "")),
                     cdf,
                     nrow = 2)
}

illustrate_RPS()

# ggsave(here::here("plots", "RPS.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 2)
