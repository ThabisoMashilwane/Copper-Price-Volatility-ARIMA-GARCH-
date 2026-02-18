# ===============================
# 04 - GARCH Family Estimation
# ===============================

library(MASS)
load("data/processed_data.RData")

# --- GARCH(1,1) Log-Likelihood ---
loglik_garch <- function(params, rt) {
  n <- length(rt)
  sigma2 <- numeric(n)
  sigma2[1] <- var(rt)

  for (t in 2:n) {
    sigma2[t] <- params[1] +
                 params[2] * rt[t-1]^2 +
                 params[3] * sigma2[t-1]
  }

  -sum(dnorm(rt, 0, sqrt(sigma2), log = TRUE))
}

# Fit model
fit_garch <- optim(
  c(0.0001, 0.1, 0.8),
  loglik_garch,
  rt = rt,
  method = "L-BFGS-B",
  lower = c(0, 0, 0),
  upper = c(Inf, 1, 1),
  hessian = TRUE
)

params_garch <- fit_garch$par
se_garch <- sqrt(diag(solve(fit_garch$hessian)))

save(params_garch, file = "models/garch_model.RData")
