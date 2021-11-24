# Miscellaneous illustrations

- [Ordered logistic distribution](ordered_logistic.R)
- [Gaussian distribution](gaussian.R)
- [Clustering](clustering.R)
- [Prediction xkcd style](prediction_xkcd.R)
- [Bayesian updating (coin flip)](bayesian_coin.R)
- [Forward chaining](forward_chaining.R)
- [Ranked Probability Score (RPS)](RPS.R)
- [Stickman](stickman.R)
- [Sampling a distribution](sampling.R)
- [Metrics for probabilistic forecasts](prob_metrics.R)

View the [gallery](gallery.md).

## Reproducibility

To reproduce the figures, it is recommended to first open the project (`.Rproj` file) in [RStudio IDE](https://www.rstudio.com/products/rstudio/).
The project uses [renv](https://rstudio.github.io/renv/index.html) to manage package dependencies.
To install the packages and their dependencies required to reproduce the figures, first install renv with `install.packages("renv")`, and then call `renv::restore()`.
