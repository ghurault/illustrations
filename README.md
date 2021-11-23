# Miscellaneous illustrations

- [Ordered logistic distribution](ordered_logistic.R)
- [Gaussian distribution](gaussian.R)
- [Clustering](clustering.R)
- [Prediction xkcd style](prediction_xkcd.R)
- Bayesian updating (coin flip): [static image](bayesian_coin.R) and [animation](bayesian_coin_anim.R)
- 

## Reproducibility

To reproduce the figures, it is recommended to first open the project (`.Rproj` file) in [RStudio IDE](https://www.rstudio.com/products/rstudio/).
The project uses [renv](https://rstudio.github.io/renv/index.html) to manage package dependencies.
To install the packages and their dependencies required to reproduce the figures, first install renv with `install.packages("renv")`, and then call `renv::restore()`.
