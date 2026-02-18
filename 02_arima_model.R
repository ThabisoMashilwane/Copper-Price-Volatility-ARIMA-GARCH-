# ===============================
# 02 - ARIMA Modelling
# ===============================

library(forecast)
library(tseries)

load("data/processed_data.RData")

# Stationarity Test
adf.test(copper_ts)

# Box-Cox transformation
lambda <- BoxCox.lambda(copper_ts)
trans <- BoxCox(copper_ts, lambda)

# Differencing order
d <- ndiffs(trans, test = "adf")
stationary <- diff(trans, differences = d)

adf.test(stationary)

# Auto ARIMA
auto.arima(trans, trace = TRUE)

# Best model
best_model <- Arima(trans, order = c(1, 1, 0))
summary(best_model)

save(best_model, file = "models/arima_model.RData")
