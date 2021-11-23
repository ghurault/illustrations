# Notes -------------------------------------------------------------------

# Illustrate the forward chaining procedure
# aka rolling forecast origin validation

# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart the session)

library(tidyverse)
library(HuraultMisc)

# Plot --------------------------------------------------------------------

illustrate_forward_chaining <- function(horizon = 7, n_it = 5) {
  # Args:
  # - horizon: prediction horizon
  # - n_it: number of iterations to display

  stopifnot(is_scalar_wholenumber(horizon),
            is_scalar_wholenumber(n_it))

  df <- lapply(1:n_it,
               function(it) {
                 data.frame(Iteration = it,
                            Day = 1:((it + 1) * horizon),
                            Subset = c(rep("Train", it * horizon), rep("Test", horizon))
                 )}) %>%
    bind_rows()
  lbl <- df %>%
    group_by(.data$Iteration, .data$Subset) %>%
    summarise(Day = stats::median(.data$Day)) %>%
    rename(Label = .data$Subset)

  ggplot() +
    geom_rect(data = df,
              aes_string(xmin = "Day - 1", xmax = "Day", ymin = "Iteration - .35" , ymax = "Iteration + .35", fill = "Subset"),
              alpha = .8) +
    geom_text(data = lbl,
              aes_string(x = "Day", y = "Iteration", label = "Label"),
              fontface = "bold", size = 8) +
    scale_y_continuous(breaks = 1:max(df[["Iteration"]]), trans = "reverse", expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, max(df[["Day"]]), horizon), expand = c(0, 0)) +
    scale_fill_manual(values = c("#009E73", "#F0E442")) +
    theme_classic(base_size = 15) +
    theme(axis.title.y = element_text(angle = 90,vjust = 0.5),
          legend.position = "none")
}

illustrate_forward_chaining()

# ggsave(here::here("plots", "forward_chaining.jpg"), width = 10, height = 5, units = "cm", dpi = 300, scale = 2.5)
