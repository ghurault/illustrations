# Notes -------------------------------------------------------------------

# Illustrating Bayes' rule in the context of a coin flipping experiment
# Outputting a gif (requires ImageMagick)

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

set.seed(15)

library(tidyverse)
library(animation)

N <- 600 # Number of coin flips
prob <- 0.8 # Probability of success
prior_beta <- c(5, 5) # Shape parameters of the Beta prior distribution

# Plot --------------------------------------------------------------------

# Data
df <- tibble(n = 1:N,
             outcome = rbinom(n, 1, prob)) %>%
  mutate(s = cumsum(outcome),
         f = n - s)
df <- bind_rows(tibble(n = 0, s = 0, f = 0), df)

step <- .001

pl <- lapply(0:N,
       function(i) {
         tmp <- filter(df, n == i)

         tmp %>%
           expand_grid(p = seq(0, 1, step)) %>%
           mutate(Density = dbeta(p, prior_beta[1] + s, prior_beta[2] + f)) %>%
           ggplot(aes(x = p, y = Density))+
           geom_line(lwd = 2) +
           scale_y_continuous(expand = expansion(c(0, 1))) +
           scale_x_continuous(limits = c(0, 1), expand = expansion(c(0, 0.01))) +
           labs(title = paste0("Head = ", tmp$s, "; Tail = ", tmp$f))+
           theme_classic(base_size = 15)
       })

# Save gif ----------------------------------------------------------------

saveGIF({for (i in 1:length(pl)) {print(pl[[i]])}},
        interval = .05,
        movie.name = here::here("plots", "bayesian_coin.gif"))
