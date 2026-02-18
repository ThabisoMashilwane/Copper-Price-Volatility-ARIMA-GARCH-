# ===============================
# 05 - Conditional Variance Forecast
# ===============================

load("data/processed_data.RData")
load("models/garch_model.RData")

omega <- params_garch[1]
alpha <- params_garch[2]
beta  <- params_garch[3]

n <- length(rt)
sigma2 <- numeric(n)
sigma2[1] <- var(rt)

for (t in 2:n) {
  sigma2[t] <- omega +
               alpha * rt[t-1]^2 +
               beta * sigma2[t-1]
}

plot(ts(sigma2, start = c(1990,2), frequency = 12),
     main = "Conditional Variance")
