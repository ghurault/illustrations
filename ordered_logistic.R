# Notes -------------------------------------------------------------------

# Illustrating the ordered logistic distribution

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear workspace (better to restart the session)

library(tidyverse)
library(cowplot)
library(HuraultMisc)

ordered_logistic_pmf <- function(eta, ct) {
  # Probability mass function of the ordered logistic distribution
  #
  # Args:
  # eta: location parameter
  # ct: vector of cut-offs
  #
  # Returns:
  # Vector of probabilities of size length(M) + 1

  stopifnot(is_scalar_double(eta),
            is.vector(ct, mode = "numeric"))

  M <- length(ct)

  p <- inv_logit(eta - ct[-length(ct)]) - inv_logit(eta - ct[-1])
  p <- c(1 - inv_logit(eta - ct[1]), p, inv_logit(eta - ct[length(ct)]))

  return(p)
}

# Plot --------------------------------------------------------------------

M <- 3
ct <- c(-3, 0, 5)

etas <- c(-2, 0, 2)
etas_labels <- c("eta < 0", "eta = 0", "eta > 0")

lapply(seq_along(etas),
       function(i) {
         eta <- etas[i]

         p1 <- tibble(x = seq(-10, 10, .1),
                      density = dlogis(x, location = eta, scale = 1)) %>%
           ggplot(aes(x = x, y = density)) +
           geom_line(size = 1) +
           geom_area(fill = HuraultMisc::cbbPalette[3]) +
           geom_vline(xintercept = ct) +
           scale_y_continuous(expand = expansion(mult = c(0, .1))) +
           # scale_x_continuous(breaks = ct, labels = paste0("c[", 1:M, "]")) +
           labs(x = "", y = "Density") +
           theme_classic(base_size = 15)

         p2 <- tibble(x = 0:M,
                      pmf = ordered_logistic_pmf(eta, ct)) %>%
           ggplot(aes(x = x, y = pmf)) +
           geom_col(fill = HuraultMisc::cbbPalette[3], colour = "black") +
           scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
           labs(x = "", y = "Probability") +
           theme_classic(base_size = 15)

         plot_grid(p1 + labs(title = etas_labels[i]),
                   p2,
                   ncol = 1)

       }) %>%
  plot_grid(plotlist = ., nrow = 1)

ggsave(here::here("plots", "ordered_logistic.jpg"),
       width = 13, height = 8, units = "cm", dpi = 300, scale = 2)
