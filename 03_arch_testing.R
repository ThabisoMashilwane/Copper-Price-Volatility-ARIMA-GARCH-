# ===============================
# 03 - ARCH Effect Testing
# ===============================

library(forecast)
library(moments)

load("data/processed_data.RData")

# Plot returns
plot(rt)

# ACF/PACF
Acf(rt)
Pacf(rt)

# Squared returns
sq_rt <- rt^2
Acf(sq_rt)

# McLeod-Li Test
lags <- 1:20
p_values <- sapply(lags, function(k)
  Box.test(sq_rt, lag = k, type = "Ljung-Box")$p.value)

plot(lags, p_values, type = "b")
abline(h = 0.05, col = "red")

# Distribution diagnostics
qqnorm(rt)
qqline(rt)

shapiro.test(rt)
skewness(rt)
kurtosis(rt) - 3
