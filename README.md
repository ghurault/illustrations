# Miscellaneous illustrations

To view the image gallery, go [here](https://ghurault.github.io/illustrations/gallery)!

The corresponding scripts are:

- [Bayesian updating (coin flip)](bayesian_coin.R)
- [Clustering](clustering.R)
- [Forward chaining](forward_chaining.R)
- [Gaussian distribution](gaussian.R)
- [Metrics for probabilistic forecasts](prob_metrics.R)
- [Ordered logistic distribution](ordered_logistic.R)
- [Prediction xkcd style](prediction_xkcd.R)
- [Ranked Probability Score (RPS)](RPS.R)
- [Sampling a distribution](sampling.R)
- [Stickman](stickman.R)


## Reproducibility

To reproduce the figures, it is recommended to first open the project (`.Rproj` file) in [RStudio IDE](https://www.rstudio.com/products/rstudio/).
The project uses [renv](https://rstudio.github.io/renv/index.html) to manage package dependencies.
To install the packages and their dependencies required to reproduce the figures, first install renv with `install.packages("renv")`, and then call `renv::restore()`.
