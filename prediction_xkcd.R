# Initialisation ----------------------------------------------------------

rm(list = ls()) # Clear Workspace (better to restart session)

library(tidyverse)
library(forecast)
library(xkcd) # Need to download xkcd fonts, cf. xkcd-intro vignettes

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(78)

# Data --------------------------------------------------------------------

# Brownian motion
t <- seq(0, 100, 1)
bm <- rep(50, length(t))
noise <- rnorm(length(t), mean = 0, sd = 1)
for (i in 1:(length(t) - 1)) {
  bm[i + 1] <- bm[i] + noise[i]
}
smooth <- loess(bm ~ t, data = data.frame(t, bm), span = .7) # Smooth brownian
y <- predict(smooth)

tp <- 60 # prediction time
y <- y[1:tp]

a <- ts(y)
fit <- ets(a)
autoplot(forecast(fit, h = 30))

f <- forecast(fit, h = 30, level = c(75, 99))

a <- seq(1, .95, length.out = length(f$mean))
b <- seq(1, .97, length.out = length(f$mean))

df <- data.frame(t = 1:tp,
                 y = y,
                 upper = y,
                 lower = y,
                 label = NA)
# df=rbind(df,data.frame(t=(1:length(f$mean))+tp,y=NA,upper=f$upper[,2]*a,lower=f$lower[,2]*a,label="A"))
# df=rbind(df,data.frame(t=(1:length(f$mean))+tp,y=NA,upper=f$upper[,1]*b,lower=f$lower[,1]*b,label="B"))
df <- rbind(df,
            data.frame(t = (1:length(f$mean)) + tp,
                       y = NA,
                       upper = f$upper[, 1] * a,
                       lower = f$lower[, 1] * a,
                       label = "A"))
df <- rbind(df,
            data.frame(t = (1:length(f$mean)) + tp,
                       y = NA,
                       upper = f$upper[, 1],
                       lower = f$lower[, 1],
                       label="B"))

# XKCD plot --------------------------------------------------------------------

xr <- c(0, 100) # range(df$t)
yr <- c(floor(min(df$lower)), ceiling(max(df$upper)))

p <- ggplot(data = df) +
  geom_line(aes(x = t, y = y), lwd = 1.5, colour = cbbPalette[7]) +
  geom_ribbon(aes(x = t, ymin = lower, ymax = upper, fill = label), alpha = .3) +
  scale_fill_manual(values = cbbPalette[c(7,6)]) +
  xkcdaxis(xr, yr) +
  labs(x = "Time", y = "Severity") +
  theme(text = element_text(size = 20),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
p

mapping <- aes(x, y, scale, ratioxy, angleofspine,
               anglerighthumerus, anglelefthumerus,
               anglerightradius, angleleftradius,
               anglerightleg, angleleftleg, angleofneck)

dataman <- data.frame( x = 99, y = 42.5,
                       scale = 1.5,
                       ratioxy = diff(xr) / diff(yr),
                       angleofspine =  -pi / 2 - pi / 24,
                       anglerighthumerus = -7 * pi / 12,
                       anglelefthumerus = -pi / 2 - pi / 4,
                       anglerightradius = -5 * pi / 6,
                       angleleftradius = 2 * pi / 3,
                       angleleftleg = 3 * pi / 2 + pi / 12 ,
                       anglerightleg = 3 * pi / 2 - pi / 12,
                       angleofneck = -0.42 * pi)


p + xkcdman(mapping, dataman) +
  annotate("text", x = 92, y = 42.5, label = "A", family = "xkcd") +
  annotate("text", x = 92, y = 40.5, label = "B", family = "xkcd") +
  annotate("text", x = 89, y = 45.5, label = "A or B ?", family = "xkcd") +
  xkcdline(aes(x = xbegin, y = ybegin, xend = xend, yend = yend),
           data = data.frame(xbegin = 95.5, xend = 90, ybegin = 43.4, yend = 45), xjitteramount = 0.12)

# ggsave(here::here("plots", "prediction_xkcd.jpg"), width = 13, height = 8, units = "cm", dpi = 300, scale = 1.1)
