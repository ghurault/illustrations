# Notes -------------------------------------------------------------------

# Illustration of sampling a posterior distribution
# - Using grid sampling
# - Using MCMC (HMC in Stan)

# Initialisation ----------------------------------------------------------

rm(list=ls()) # Clear Workspace (better to restart session)

set.seed(15)

library(tidyverse)
library(rstan)
library(gganimate)

N <- 200 # Number of datapoints

# ... --------------------------------------------------------------------

x <- rnorm(N, mean = 1, sd = .5)

model_code <- "
data {
  int<lower = 1> N;
  real x[N];
}

parameters {
  real mu;
  real<lower = 0> tau;
}

transformed parameters {
  real sigma = 1 / sqrt(tau);
}

model {
  // Priors (conjugate)
  mu ~ normal(0, 1);
  tau ~ gamma(2, 2);

  x ~ normal(mu, sigma);
}
"

data_stan <- list(N = N, x = x)
fit <- stan(model_code = model_code, data = data_stan, iter = 2000, chains = 1)

# pairs(fit)
# print(fit)

post <- as.data.frame(fit)

# Parameter space ---------------------------------------------------------

mu_lims <- c(0.5, 1.5)
sigma_lims = c(0, 1)

estGammaParams <- function(mu, std) {
  alpha <- (mu / std)^2
  beta <- mu / (std^2)
  return(list(alpha = alpha, beta = beta))
}

# Approximate posterior distribution
tau_gamma <- estGammaParams(mean(post$tau), sd(post$tau))
df <- expand_grid(mu = seq(0.5, 1.5, length.out = 100),
                  sigma = seq(0, 1, length.out = 100)) %>%
  mutate(tau = 1 / sigma^2,
         density_mu = dnorm(mu, mean = mean(post$mu), sd = sd(post$mu)),
         density_tau = dgamma(tau, shape = tau_gamma$alpha, rate = tau_gamma$beta),
         density =  density_mu * density_tau)

p0 <- ggplot(data = df) +
  geom_raster(aes(x = mu, y = sigma, fill = density)) +
  coord_cartesian(expand = FALSE) +
  scale_fill_distiller(palette = "Spectral") +
  theme_classic(base_size = 15) +
  theme(legend.position = "none")

# Grid sampling -----------------------------------------------------------

p1 <- p0 +
  geom_point(data = expand.grid(x = seq(mu_lims[1], mu_lims[2], length.out = 10),
                                y = seq(sigma_lims[1], sigma_lims[2], length.out = 10)),
             aes(x = x, y = y))
p1

# ggsave(here::here("plots", "grid_sampling.jpg"), width = 12, height = 9, units = "cm", dpi = 300, scale = 2)

# MCMC sampling -----------------------------------------------------------

tr <- rstan::extract(fit, pars = c("mu", "sigma"), permuted = FALSE, inc_warmup = TRUE)[, 1, ] %>%
  as.data.frame() %>%
  mutate(Index = 1:n()) %>%
  head(100)
p2 <- p0 +
  geom_path(data = tr, aes(x = mu, y = sigma), alpha = .5) +
  geom_point(data = tr, aes(x = mu, y = sigma, group = seq_along(Index)))
p2

# ggsave(here::here("plots", "mcmc_sampling.jpg"), width = 12, height = 9, units = "cm", dpi = 300, scale = 2)

# Animation
animate(p2 + transition_reveal(Index),
        width = 900,
        height = 600,
        units = "px")

# anim_save(filename = here::here("plots", "mcmc_sampling.gif"))
