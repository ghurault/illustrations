# Notes -------------------------------------------------------------------

# Illustrating Bayes' rule in the context of a coin flipping experiment

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

set.seed(15)

library(tidyverse)

N <- 5 # Number of coin flips
prob <- 0.8 # Probability of success
prior_beta <- c(5, 5) # Shape parameters of the Beta prior distribution

# Plot --------------------------------------------------------------------

# Data
s <- sum(rbinom(N, size = 1, prob = prob)) # Number of success
f <- N - s # Number of failures

step <- .001

# Prior, likelihood and posterior
df <- tibble(p = seq(0, 1, step)) %>%
  mutate(Prior = dbeta(p, prior_beta[1], prior_beta[2]),
         Likelihood = (p^s) * ((1 - p)^f),
         Likelihood = Likelihood / sum(Likelihood * Prior * step), # # normalise Likelihood by p(x)
         Posterior = dbeta(p, prior_beta[1] + s, prior_beta[2] + f)) %>%
  pivot_longer(!p, names_to = "Distribution", values_to = "Density")

plot_distribution <- function(df, dis, N){
  df %>%
    filter(Distribution %in% dis) %>%
    ggplot(aes(x = p, y = Density, colour = Distribution)) +
    geom_line(lwd = 2) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, max(df[["Density"]]) * 1.1)) +
    scale_x_continuous(limits = c(0, 1), expand = expansion(c(0, 0.01))) +
    scale_color_manual(values = HuraultMisc::cbbPalette) +
    labs(title = paste0("N=", N), x = expression(theta), colour = "") +
    theme_classic(base_size = 20) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "top")
}

# plot_distribution(df, c("Prior"), 0)
# plot_distribution(df, c("Prior", "Likelihood"), N)
plot_distribution(df, c("Prior", "Likelihood", "Posterior"), N)

# ggsave(here::here("plots", "bayesian_coin.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 2)
