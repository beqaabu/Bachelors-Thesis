# Read data

library(ggplot2)
library(tseries)
library(forecast)
data <- read.csv("/home/beka/Desktop/inflation.csv")
print(data)

# Transform into time series
rate <- ts(data[, 'Inflation'], start = c(1996,1), frequency = 12)
print(rate)

# Plot
autoplot(rate) + ggtitle("Inflation Rate") + ylab("Inflation")

# Build arima model
fit_model = auto.arima(rate, seasonal = TRUE)
print(summary(fit_model))
checkresiduals(fit_model)


for (i in 0:6){
  print(adf.test(rate, k = i))
}
adf.test(rate, k = 24)

pacf(data, 20, TRUE)
# Forecast
fcast <- forecast(fit_model, h = 6)
autoplot
plot(fcast)
print(summary(fcast))

stat.desc(rate)
acf(data)


data$Data
data
x2 <- seq(-10, 25, length = 40)

# Normal curve
fun <- dnorm(x2, mean = mean(rate), sd = sd(rate))

# Histogram
hist(rate, prob = TRUE,
     ylim = c(0, max(fun)),
     col = "darkmagenta", xlab = "Inflation Rate", main = "Histogram of Georgian Inflation")
lines(x2, fun, col = "green", lwd = 2)
