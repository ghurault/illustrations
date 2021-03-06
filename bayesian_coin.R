# Notes -------------------------------------------------------------------

# Illustrating Bayes' rule in the context of a coin flipping experiment

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

set.seed(15)

library(tidyverse)
library(gganimate)

prob <- 0.8 # Probability of success
prior_beta <- c(5, 5) # Shape parameters of the Beta prior distribution
step <- 0.005 # x-axis resolution

# Static plot --------------------------------------------------------------------

# Data
N <- 5 # Number of coin flips
s <- sum(rbinom(N, size = 1, prob = prob)) # Number of success
f <- N - s # Number of failures

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

# Animation ---------------------------------------------------------------

# Could also use transition_states since it's n is discrete
# But because N is big, it does not matter much

# Data
N <- 400
df <- tibble(n = 1:N,
             outcome = rbinom(n, 1, prob)) %>%
  mutate(s = cumsum(outcome),
         f = n - s)
df <- bind_rows(tibble(n = 0, s = 0, f = 0), df)

tmp <- expand_grid(n = 0:N, p = seq(0, 1, step)) %>%
  inner_join(df, by = "n") %>%
  mutate(Density = dbeta(p, prior_beta[1] + s, prior_beta[2] + f))

p <- tmp %>%
  ggplot(aes(x = p, y = Density)) +
  geom_line(lwd = 2) +
  transition_time(n) +
  # view_follow(fixed_x = TRUE) +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_x_continuous(limits = c(0, 1), expand = expansion(c(0, 0.01))) +
  labs(title = paste0("Heads = {df %>% filter(n == round(frame_time)) %>% pull(s)}
                      Tails = {df %>% filter(n == round(frame_time)) %>% pull(f)}"),
       x = expression(theta)) +
  theme_classic(base_size = 15)

animate(p,
        nframes = N,
        fps = 25,
        width = 800,
        height = 500,
        units = "px")
anim_save(filename = here::here("plots", "bayesian_coin.gif"))
