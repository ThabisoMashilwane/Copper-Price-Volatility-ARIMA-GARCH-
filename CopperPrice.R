# Load required libraries
library(readxl)    
library(forecast)   
library(tseries)    
library(MASS)
library(TSA)

# Step 1: Data Preparation
# Read the Excel file and extract the monthly data
data <- read_excel("C:/Users/thabi/Downloads/PCOPPUSDM (1).xlsx",sheet = "Monthly")

# Convert observation_date from Excel serial to Date format
data$observation_date <- as.Date(data$observation_date, origin = "1899-12-30")

# Sort data by date to ensure chronological order (though it should already be)
data <- data[order(data$observation_date), ]

# Create time series object for copper prices (monthly frequency starting from 1990-01)
copper_ts <- ts(data$PCOPPUSDM, start = c(1990, 1), frequency = 12)

# Step 2: ARIMA Model Identification and Estimation
#Stationarity Testing
adf.test(copper_ts)

# Stationarity testing through Box-Cox transformation
# Estimate lambda for Box-Cox transformation
lambda <- BoxCox.lambda(copper_ts)
win.graph(width=3, height=3, pointsize=8)
plot(BoxCox(copper_ts, BoxCox.lambda(copper_ts)))
cat("Estimated Box-Cox lambda:", lambda, "\n")

# Apply Box-Cox transformation
trans <- BoxCox(copper_ts, lambda)

# Determine the order of differencing (d) using ADF test
d <- ndiffs(trans, alpha = 0.05, test = "adf")
cat("Order of differencing (d):", d, "\n")

# Difference the transformed series to achieve stationarity
stationary <- diff(trans, differences = d)

# Identify ARMA(p, q) using ACF and PACF (for visual inspection)
win.graph(width=3, height=3, pointsize=8)
Acf(stationary, main = "ACF of Stationary Series")
win.graph(width=3, height=3, pointsize=8)
Pacf(stationary, main = "PACF of Stationary Series")

adf.test(stationary)

# Auto-ARIMA
auto.arima(trans, trace = TRUE)

#Overfitting and Underfitting
a <- Arima(trans, order = c(2, 1, 2),seasonal = list(order = c(1, 0, 1),period = 12),include.drift = TRUE)
b <- Arima(trans, order = c(0, 1, 0),include.drift = TRUE)
c <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(1, 0, 0),period = 12),include.drift = TRUE)
d <- Arima(trans, order = c(0, 1, 1),seasonal = list(order = c(0, 0, 1),period = 12),include.drift = TRUE)
e <- Arima(trans, order = c(0, 1, 0))
f <- Arima(trans, order = c(1, 1, 0),include.drift = TRUE)
g <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(2, 0, 0),period = 12),include.drift = TRUE)
h <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(1, 0, 1),period = 12),include.drift = TRUE)
i <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(0, 0, 1),period = 12),include.drift = TRUE)
j <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(2, 0, 1),period = 12),include.drift = TRUE)
k <- Arima(trans, order = c(0, 1, 0),seasonal = list(order = c(1, 0, 0),period = 12),include.drift = TRUE)
l <- Arima(trans, order = c(2, 1, 0),seasonal = list(order = c(1, 0, 0),period = 12),include.drift = TRUE)
m <- Arima(trans, order = c(1, 1, 1),seasonal = list(order = c(1, 0, 0),period = 12),include.drift = TRUE)
n <- Arima(trans, order = c(0, 1, 1),seasonal = list(order = c(1, 0, 0),period = 12),include.drift = TRUE)
o <- Arima(trans, order = c(2, 1, 1),seasonal = list(order = c(1, 0, 0),period = 12),include.drift = TRUE)
p <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(1, 0, 0),period = 12))
q <- Arima(trans, order = c(1, 1, 0))
r <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(2, 0, 0),period = 12))
s <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(1, 0, 1),period = 12))
t <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(0, 0, 1),period = 12))
u <- Arima(trans, order = c(1, 1, 0),seasonal = list(order = c(2, 0, 1),period = 12))
v <- Arima(trans, order = c(0, 1, 0),seasonal = list(order = c(1, 0, 0),period = 12))
w <- Arima(trans, order = c(2, 1, 0),seasonal = list(order = c(1, 0, 0),period = 12))
x <- Arima(trans, order = c(1, 1, 1),seasonal = list(order = c(1, 0, 0),period = 12))
y <- Arima(trans, order = c(0, 1, 1),seasonal = list(order = c(1, 0, 0),period = 12))
z <- Arima(trans, order = c(2, 1, 1),seasonal = list(order = c(1, 0, 0),period = 12))

#Testing AIC's and BIC's
BIC(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)
AIC(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z)

####### Best Model is ARIMA(1,1,0)
# Model Fitting
best_model <- Arima(trans, order = c(1, 1, 0))
best_model

# Step 3: Model Diagnostics

# Extract standardized residuals
resids <- residuals(best_model)

# Plot standardized residuals
plot(resids, ylab = "Standardized Residuals", main = "Standardized Residuals Plot")
abline(h = 0, col = "red")

# Assess residual normality using Q-Q plot
qqnorm(resids, main = "Q-Q Plot of Residuals")
qqline(resids, col = "red")

# Shapiro-Wilk test for normality
shapiro_result <- shapiro.test(resids)
print(shapiro_result)

# Assess correlations of residuals through ACF
Acf(resids, main = "ACF of Residuals")

# Ljung-Box test for residual autocorrelation
ljung_box <- Box.test(resids, lag = 20, type = "Ljung-Box")
print(ljung_box)

#################################################################################
#ARCH-GARCH
# Load required libraries
library(readxl)     # For reading Excel files
library(forecast)   # For ACF/PACF, Box.test, etc.
library(tseries)   # For garch function and additional tests
#install.packages("moments")
library(moments)    # For skewness and kurtosis
library(nortest)    # For Jarque-Bera test (jarque.test)

# Step 1: Data Preparation
# Read the Excel file and extract the monthly data
data <- read_excel("C:/Users/thabi/Downloads/PCOPPUSDM (1).xlsx",sheet = "Monthly")

# Convert observation_date from Excel serial to Date format
data$observation_date <- as.Date(data$observation_date, origin = "1899-12-30")

# Sort data by date to ensure chronological order
data <- data[order(data$observation_date), ]

# Create time series object for copper prices (monthly frequency starting from 1990-01)
copper_ts <- ts(data$PCOPPUSDM, start = c(1990, 1), frequency = 12)

# Compute log returns (rt = log(pt) - log(pt-1))
log_prices <- log(copper_ts)
rt <- diff(log_prices)  # Returns (can multiply by 100 for percentage if desired: rt <- 100 * diff(log_prices))

# Step 2: Exploratory Analysis (Volatility Clustering and Dependence)
# Plot the price series (Exhibit 12.1 equivalent)
plot(copper_ts, main = "Time Plot of Copper Prices", ylab = "Price (USD per Metric Ton)")

# Plot the returns series (Exhibit 12.2 equivalent)
win.graph(width=3, height=3, pointsize=8)
plot(rt, main = "Time Plot of Copper Log Returns", ylab = "Log Return")
abline(h=0)
# ACF and PACF of returns (Exhibits 12.3 and 12.4)
Acf(rt, main = "ACF of Copper Log Returns")
Pacf(rt, main = "PACF of Copper Log Returns")

# ACF and PACF of absolute returns (Exhibits 12.5 and 12.6)
abs_rt <- abs(rt)
Acf(abs_rt, main = "ACF of Absolute Copper Log Returns")
Pacf(abs_rt, main = "PACF of Absolute Copper Log Returns")

# ACF and PACF of squared returns (Exhibits 12.7 and 12.8)
sq_rt <- rt^2
Acf(sq_rt, main = "ACF of Squared Copper Log Returns")
Pacf(sq_rt, main = "PACF of Squared Copper Log Returns")

# McLeod-Li Test for ARCH effects (Exhibit 12.9) - Ljung-Box on squared returns
# Plot p-values for different lags
#H0: There is NO ARCH in the data
lags <- 1:20
p_values <- sapply(lags, function(k) Box.test(sq_rt, lag = k, type = "Ljung-Box")$p.value)
plot(lags, p_values, type = "b", ylim = c(0, 1), main = "McLeod-Li Test p-values", ylab = "p-value", xlab = "Lag")
abline(h = 0.05, col = "red", lty = 2)

# Distributional Analysis (Page 3)
# QQ Plot (Exhibit 12.10)
qqnorm(rt, main = "QQ Normal Plot of Copper Log Returns")
qqline(rt, col = "red")

# Shapiro-Wilk Test
shapiro.test(rt)

# Skewness and Kurtosis (excess kurtosis = kurtosis - 3)
skewness(rt)
kurtosis(rt) - 3  # Excess kurtosis as in PDF

# Step 3: Define Log-Likelihood Functions with Fixes
loglik_garch <- function(params, rt) {
  n <- length(rt)
  sigma2 <- numeric(n)
  sigma2[1] <- max(var(rt), 1e-6)
  for (t in 2:n) {
    sigma2[t] <- params[1] + params[2] * rt[t-1]^2 + params[3] * sigma2[t-1]
    sigma2[t] <- max(sigma2[t], 1e-6)
  }
  -sum(dnorm(rt, mean = 0, sd = sqrt(pmax(sigma2, 1e-6)), log = TRUE))  # Ensure finite sd
}

loglik_egarch <- function(params, rt) {
  n <- length(rt)
  sigma2 <- numeric(n)
  sigma2[1] <- max(var(rt), 1e-6)
  for (t in 2:n) {
    z_t1 <- if (sigma2[t-1] > 0) rt[t-1] / sqrt(sigma2[t-1]) else 0
    log_sigma2 <- params[1] + params[2] * z_t1 + params[3] * log(max(sigma2[t-1], 1e-6))
    sigma2[t] <- exp(pmin(pmax(log_sigma2, -10), 10))  # Clamp to avoid extreme values
  }
  -sum(dnorm(rt, mean = 0, sd = sqrt(pmax(sigma2, 1e-6)), log = TRUE))
}

loglik_gjr <- function(params, rt) {
  n <- length(rt)
  sigma2 <- numeric(n)
  sigma2[1] <- max(var(rt), 1e-6)
  for (t in 2:n) {
    gamma <- params[4]
    sigma2[t] <- params[1] + params[2] * rt[t-1]^2 + params[3] * sigma2[t-1] + 
      gamma * rt[t-1]^2 * (rt[t-1] < 0)
    sigma2[t] <- max(sigma2[t], 1e-6)
  }
  -sum(dnorm(rt, mean = 0, sd = sqrt(pmax(sigma2, 1e-6)), log = TRUE))
}

loglik_aparch <- function(params, rt) {
  n <- length(rt)
  sigma <- numeric(n)
  sigma[1] <- max(sd(rt), 1e-6)
  delta <- params[5]
  for (t in 2:n) {
    z_t1 <- abs(rt[t-1]) / sigma[t-1]
    sigma[t] <- (params[1] + params[2] * (abs(rt[t-1]) - params[4] * rt[t-1])^delta + 
                   params[3] * sigma[t-1]^delta)^(1/delta)
    sigma[t] <- max(sigma[t], 1e-6)
  }
  -sum(dnorm(rt, mean = 0, sd = pmax(sigma, 1e-6), log = TRUE))
}

# Step 4: Fit Models Manually Using optim
# GARCH(1,1)
init_garch <- c(0.0001, 0.1, 0.8)
fit_garch <- optim(init_garch, loglik_garch, rt = rt, method = "L-BFGS-B", 
                   lower = c(0, 0, 0), upper = c(Inf, 1, 1), hessian = TRUE)
params_garch <- fit_garch$par
se_garch <- sqrt(diag(solve(fit_garch$hessian)))
cat("GARCH(1,1) Parameters:", params_garch, "\n")
cat("GARCH(1,1) Standard Errors:", se_garch, "\n")

# EGARCH(1,1) 
loglik_egarch <- function(params, rt) {
  omega <- params[1]
  alpha <- params[2]
  gamma <- params[3]
  beta <- params[4]
  n <- length(rt)
  sigma2 <- rep(var(rt), n)  # Initialize with unconditional variance
  loglik <- 0
  
  for (t in 2:n) {
    z <- rt[t-1] / sqrt(max(sigma2[t-1], 1e-10))  # Avoid division by zero
    log_sigma2 <- omega + alpha * (abs(z) - sqrt(2/pi)) + gamma * z + beta * log(max(sigma2[t-1], 1e-10))
    sigma2[t] <- exp(log_sigma2)
    loglik <- loglik - 0.5 * (log(2 * pi) + log(max(sigma2[t], 1e-10)) + rt[t]^2 / max(sigma2[t], 1e-10))
  }
  
  if (!is.finite(loglik)) return(1e10)  # Penalize non-finite likelihood
  return(-loglik)  # Return negative log-likelihood
}

# EGARCH(1,1) with four parameters
init_egarch <- c(-0.1, 0.05, 0, 0.98)  # Adjusted initials based on your output
fit_egarch <- optim(init_egarch, loglik_egarch, rt = rt, method = "L-BFGS-B", 
                    lower = c(-5, -5, -1, 0), upper = c(5, 5, 1, 1), 
                    control = list(maxit = 5000, factr = 1e7, trace = 2), hessian = TRUE)
params_egarch <- fit_egarch$par

# Check convergence
if (fit_egarch$convergence == 0) {
  cat("EGARCH(1,1) Optimization converged successfully\n")
} else {
  cat("EGARCH(1,1) Optimization failed, convergence code:", fit_egarch$convergence, "\n")
}

# Check Hessian eigenvalues
hess_eigen <- eigen(fit_egarch$hessian, only.values = TRUE)$values
cat("EGARCH(1,1) Hessian eigenvalues:", hess_eigen, "\n")
if (any(hess_eigen <= 0)) {
  cat("Warning: Non-positive definite Hessian detected\n")
}

# Compute standard errors
se_egarch <- tryCatch(
  {
    sqrt(diag(solve(fit_egarch$hessian)))
  },
  error = function(e) {
    cat("Error in Hessian inversion, using generalized inverse\n")
    sqrt(diag(MASS::ginv(fit_egarch$hessian)))
  }
)

# Check for NaN in standard errors
if (any(is.nan(se_egarch))) {
  cat("Warning: NaN detected in EGARCH(1,1) standard errors. Check Hessian or optimization.\n")
}

cat("EGARCH(1,1) Parameters (omega, alpha, gamma, beta):", params_egarch, "\n")
cat("EGARCH(1,1) Standard Errors:", se_egarch, "\n")

# GJR-GARCH(1,1)
init_gjr <- c(0.0002, 0.15, 0.75, 0.15)  # Adjusted initials to avoid boundary issues
fit_gjr <- optim(init_gjr, loglik_gjr, rt = rt, method = "L-BFGS-B", 
                 lower = c(1e-6, 1e-4, 1e-4, -0.5), upper = c(Inf, 0.999, 0.999, 0.999), 
                 control = list(maxit = 5000, factr = 1e7, trace = 2), hessian = TRUE)
params_gjr <- fit_gjr$par

# Check convergence
if (fit_gjr$convergence == 0) {
  cat("GJR-GARCH(1,1) Optimization converged successfully\n")
} else {
  cat("GJR-GARCH(1,1) Optimization failed, convergence code:", fit_gjr$convergence, "\n")
}

# Check Hessian eigenvalues
hess_eigen <- eigen(fit_gjr$hessian, only.values = TRUE)$values
cat("GJR-GARCH(1,1) Hessian eigenvalues:", hess_eigen, "\n")
if (any(hess_eigen <= 0)) {
  cat("Warning: Non-positive definite Hessian detected\n")
}

# Compute standard errors with generalized inverse if Hessian is not positive definite
se_gjr <- tryCatch(
  {
    if (all(hess_eigen > 0)) {
      sqrt(diag(solve(fit_gjr$hessian)))
    } else {
      cat("Hessian not positive definite, using generalized inverse\n")
      sqrt(diag(MASS::ginv(fit_gjr$hessian)))
    }
  },
  error = function(e) {
    cat("Error in Hessian inversion, using generalized inverse\n")
    sqrt(diag(MASS::ginv(fit_gjr$hessian)))
  }
)

# Check for NaN in standard errors
if (any(is.nan(se_gjr))) {
  cat("Warning: NaN detected in GJR-GARCH(1,1) standard errors. Check Hessian or optimization.\n")
  # Attempt to refine with adjusted initials if NaN persists
  init_gjr_adj <- c(0.0001, 0.1, 0.8, 0.1)  # Revert to original if needed
  fit_gjr_adj <- optim(init_gjr_adj, loglik_gjr, rt = rt, method = "L-BFGS-B", 
                       lower = c(1e-6, 1e-4, 1e-4, -0.5), upper = c(Inf, 0.999, 0.999, 0.999), 
                       control = list(maxit = 5000, factr = 1e7, trace = 2), hessian = TRUE)
  params_gjr <- fit_gjr_adj$par
  hess_eigen_adj <- eigen(fit_gjr_adj$hessian, only.values = TRUE)$values
  if (all(hess_eigen_adj > 0)) {
    se_gjr <- sqrt(diag(solve(fit_gjr_adj$hessian)))
  } else {
    se_gjr <- sqrt(diag(MASS::ginv(fit_gjr_adj$hessian)))
  }
  cat("Adjusted GJR-GARCH(1,1) Parameters:", params_gjr, "\n")
  cat("Adjusted GJR-GARCH(1,1) Standard Errors:", se_gjr, "\n")
}

cat("GJR-GARCH(1,1) Parameters:", params_gjr, "\n")
cat("GJR-GARCH(1,1) Standard Errors:", se_gjr, "\n")

# APARCH(1,1)
init_aparch <- c(0.0001, 0.1, 0.8, 0.1, 1.5)
fit_aparch <- optim(init_aparch, loglik_aparch, rt = rt, method = "L-BFGS-B", 
                    lower = c(0, 0, 0, -1, 0.1), upper = c(Inf, 1, 1, 1, 5), 
                    hessian = TRUE)
params_aparch <- fit_aparch$par
se_aparch <- sqrt(diag(solve(fit_aparch$hessian)))
cat("APARCH(1,1) Parameters:", params_aparch, "\n")
cat("APARCH(1,1) Standard Errors:", se_aparch, "\n")

# Step 5: Compute Conditional Variances
sigma2_garch <- numeric(length(rt))
sigma2_garch[1] <- max(var(rt), 1e-6)
for (t in 2:length(rt)) {
  sigma2_garch[t] <- params_garch[1] + params_garch[2] * rt[t-1]^2 + params_garch[3] * sigma2_garch[t-1]
  sigma2_garch[t] <- max(sigma2_garch[t], 1e-6)
}

sigma2_egarch <- numeric(length(rt))
sigma2_egarch[1] <- max(var(rt), 1e-6)
for (t in 2:length(rt)) {
  z_t1 <- if (sigma2_egarch[t-1] > 0) rt[t-1] / sqrt(sigma2_egarch[t-1]) else 0
  log_sigma2 <- params_egarch[1] + params_egarch[2] * z_t1 + params_egarch[3] * log(max(sigma2_egarch[t-1], 1e-6))
  sigma2_egarch[t] <- exp(pmin(pmax(log_sigma2, -10), 10))
}

sigma2_gjr <- numeric(length(rt))
sigma2_gjr[1] <- max(var(rt), 1e-6)
for (t in 2:length(rt)) {
  gamma <- params_gjr[4]
  sigma2_gjr[t] <- params_gjr[1] + params_gjr[2] * rt[t-1]^2 + params_gjr[3] * sigma2_gjr[t-1] + 
    gamma * rt[t-1]^2 * (rt[t-1] < 0)
  sigma2_gjr[t] <- max(sigma2_gjr[t], 1e-6)
}

sigma_aparch <- numeric(length(rt))
sigma_aparch[1] <- max(sd(rt), 1e-6)
delta <- params_aparch[5]
for (t in 2:length(rt)) {
  z_t1 <- abs(rt[t-1]) / sigma_aparch[t-1]
  sigma_aparch[t] <- (params_aparch[1] + params_aparch[2] * (abs(rt[t-1]) - params_aparch[4] * rt[t-1])^delta + 
                        params_aparch[3] * sigma_aparch[t-1]^delta)^(1/delta)
  sigma_aparch[t] <- max(sigma_aparch[t], 1e-6)
}

# Step 6: Model Diagnostics
models <- c("GARCH(1,1)", "EGARCH(1,1)", "GJR-GARCH(1,1)", "APARCH(1,1)")
sigmas <- list(sigma2_garch, sigma2_egarch, sigma2_gjr, sigma_aparch)

for (i in 1:length(models)) {
  cat("\n=== Diagnostics for", models[i], "===\n")
  std_resids <- rt / sqrt(sigmas[[i]][2:length(rt)])  # Align lengths by using correct indices
  
  # Plot standardized residuals
  plot(std_resids, main = paste("SR from -", models[i]), ylab = "Standardized Residual")
  abline(h = 0, col = "red")
  
  # QQ Plot
  qqnorm(std_resids, main = paste("QQ Plot -", models[i]))
  qqline(std_resids, col = "red")
  
  # Shapiro-Wilk Test
  cat("\nShapiro-Wilk Test:\n")
  print(shapiro.test(as.vector(std_resids)))
  
  # Jarque-Bera Test
  cat("\nJarque-Bera Test:\n")
  print(jarque.test(as.vector(std_resids)))
  
  # Skewness and Excess Kurtosis
  cat("Skewness:", skewness(std_resids), "\n")
  cat("Excess Kurtosis:", kurtosis(std_resids) - 3, "\n")
  
  # ACF/PACF of squared and absolute standardized residuals
  sq_std_resids <- std_resids^2
  abs_std_resids <- abs(std_resids)
  Acf(sq_std_resids, main = paste("ACF of SR2 -", models[i]))
  Pacf(sq_std_resids, main = paste("PACF of SR2 -", models[i]))
  Acf(abs_std_resids, main = paste("ACF of Abs SR -", models[i]))
  Pacf(abs_std_resids, main = paste("PACF of Abs SR -", models[i]))
  
  # Portmanteau p-values for squared std residuals
  p_values_sq <- sapply(lags, function(k) Box.test(sq_std_resids, lag = k, type = "Box-Pierce")$p.value)
  plot(lags, p_values_sq, type = "b", ylim = c(0, 1), main = paste("Portmanteau test SR2 -", models[i]), 
       ylab = "p-value", xlab = "Lag")
  abline(h = 0.05, col = "red", lty = 2)
  
  # Portmanteau p-values for absolute std residuals
  p_values_abs <- sapply(lags, function(k) Box.test(abs_std_resids, lag = k, type = "Box-Pierce")$p.value)
  plot(lags, p_values_abs, type = "b", ylim = c(0, 1), main = paste("Portmanteau test Abs SR -", models[i]), 
       ylab = "p-value", xlab = "Lag")
  abline(h = 0.05, col = "red", lty = 2)
}

# Step 7: Compare Models
loglik_values <- c(-fit_garch$value, -fit_egarch$value, -fit_gjr$value, -fit_aparch$value)
if (any(is.na(loglik_values))) loglik_values[is.na(loglik_values)] <- Inf  # Handle NA from failed fit
k_params <- c(3, 3, 4, 5)  # Number of parameters
n <- length(rt)
aic <- -2 * loglik_values + 2 * k_params
bic <- -2 * loglik_values + k_params * log(n)
cat("\n=== Model Comparison ===\n")
cat("Model\tAIC\tBIC\n")
for (i in 1:length(models)) {
  cat(models[i], "\t", round(aic[i], 4), "\t", round(bic[i], 4), "\n")
}
best_aic <- models[which.min(aic)]
cat("Best model based on AIC:", best_aic, "\n")

#Step 8: Forecasting Future Conditional Variance
# Ensure required libraries are loaded
library(forecast)  # For ts object handling

# Parameters from EGARCH(1,1) fit
omega <- params_egarch[1]  # -0.3585834
alpha <- params_egarch[2]  # 0.2277245
gamma <- params_egarch[3]  # -0.00394489
beta <- params_egarch[4]   # 0.9361736

# Length of rt and existing sigma2_egarch
n <- length(rt)
sigma2_egarch <- numeric(n)
sigma2_egarch[1] <- max(var(rt), 1e-6)  # Initial variance

# Recalculate existing conditional variances to ensure consistency
for (t in 2:n) {
  z_t1 <- rt[t-1] / sqrt(max(sigma2_egarch[t-1], 1e-6))
  log_sigma2 <- omega + alpha * (abs(z_t1) - sqrt(2/pi)) + gamma * z_t1 + beta * log(max(sigma2_egarch[t-1], 1e-6))
  sigma2_egarch[t] <- exp(log_sigma2)
}

# Forecast conditional variance for the next 24 periods with simulation for prediction limits
forecast_horizon <- 24
n_sim <- 1000  # Number of simulations for confidence intervals
sigma2_forecast_sim <- matrix(0, nrow = forecast_horizon, ncol = n_sim)

for (sim in 1:n_sim) {
  sigma2_sim <- sigma2_egarch[n]  # Start with the last observed variance
  for (t in 1:forecast_horizon) {
    z_t <- rnorm(1, 0, 1)  # Simulate future standardized innovation from N(0,1)
    log_sigma2 <- omega + alpha * (abs(z_t) - sqrt(2/pi)) + gamma * z_t + beta * log(max(sigma2_sim, 1e-6))
    sigma2_sim <- exp(log_sigma2)
    sigma2_forecast_sim[t, sim] <- sigma2_sim
  }
}

# Compute mean forecast and 95% prediction limits
sigma2_forecast_mean <- rowMeans(sigma2_forecast_sim)
sigma2_forecast_lower <- apply(sigma2_forecast_sim, 1, quantile, probs = 0.025)
sigma2_forecast_upper <- apply(sigma2_forecast_sim, 1, quantile, probs = 0.975)

# Combine historical and forecasted variances
total_variance_mean <- c(sigma2_egarch, sigma2_forecast_mean)
total_variance_lower <- c(sigma2_egarch, sigma2_forecast_lower)
total_variance_upper <- c(sigma2_egarch, sigma2_forecast_upper)

# Time indices
time_index <- time(copper_ts)[1:length(sigma2_egarch)]  # Historical time
forecast_time <- seq(time_index[length(time_index)] + 1/12, by = 1/12, length.out = forecast_horizon)
full_time <- c(time_index, forecast_time)

# Create ts objects
variance_mean_ts <- ts(total_variance_mean, start = start(copper_ts), frequency = 12)
variance_lower_ts <- ts(total_variance_lower, start = start(copper_ts), frequency = 12)
variance_upper_ts <- ts(total_variance_upper, start = start(copper_ts), frequency = 12)

# Plot the conditional variance with shaded prediction limits
plot(variance_mean_ts, type = 'l', ylab = 'Conditional Variance', xlab = 'Time', 
     main = 'EGARCH(1,1) Conditional Variance Forecast for Copper Prices')
abline(v = time_index[length(time_index)], col = "red", lty = 2)  # Vertical line at forecast start

# Shade prediction limits from forecast start
forecast_start_idx <- length(sigma2_egarch) + 1
polygon(c(full_time[forecast_start_idx:length(full_time)], rev(full_time[forecast_start_idx:length(full_time)])),
        c(total_variance_lower[forecast_start_idx:length(full_time)], rev(total_variance_upper[forecast_start_idx:length(full_time)])),
        col = rgb(0.8, 0.8, 0.8, 0.5), border = NA)  # Gray shading with transparency

# Re-draw the mean forecast line over the shading
lines(variance_mean_ts[forecast_start_idx:length(full_time)], col = "blue", lty = 1)

# Updated legend including the red dotted line
legend("topright", legend = c("Historical", "Forecast Mean", "95% Prediction Limits", "Forecast Start"), 
       lty = c(1, 1, NA, 2), col = c("black", "blue", NA, "red"), 
       fill = c(NA, NA, rgb(0.8, 0.8, 0.8, 0.5), NA), 
       border = c(NA, NA, "black", NA), bty = "n")
