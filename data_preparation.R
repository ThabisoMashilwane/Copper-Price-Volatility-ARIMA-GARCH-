# ===============================
# 01 - Data Preparation
# ===============================

library(readxl)

# Load data
data <- read_excel("data/PCOPPUSDM.xlsx", sheet = "Monthly")

# Convert date
data$observation_date <- as.Date(data$observation_date, origin = "1899-12-30")

# Sort chronologically
data <- data[order(data$observation_date), ]

# Create monthly time series
copper_ts <- ts(data$PCOPPUSDM, start = c(1990, 1), frequency = 12)

# Log returns for volatility modelling
log_prices <- log(copper_ts)
rt <- diff(log_prices)

save(copper_ts, rt, file = "data/processed_data.RData")
