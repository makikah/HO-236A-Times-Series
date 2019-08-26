## Processos Estacionários

N = 1000
set.seed(12345)

# Modelo AR(1)

y1 = arima.sim(model = list(order(1,0,0), ar = 0,9), n = N)
y1

par(mfrow = c(3, 1))

plot.ts(y1, xlab = "tempo", ylab = "observações")
acf(y1, main = expression(paste("AR(1) com", phi, "= 0.9")), xlab = "defasagem", 
    ylab = "FAC")
pacf(y1, main = expression(paste("AR(1) com", phi, "= 0.9")), xlab = "defasagem", 
    ylab = "PFAC")

y2 = arima.sim(model = list(order(1,0,0), ar = -0,8), n = N)
y2

par(mfrow = c(3, 1))

plot.ts(y2, xlab = "tempo", ylab = "observações")
acf(y2, main = expression(paste("AR(1) com", phi, "= -0.8")), xlab = "defasagem", 
    ylab = "FAC")
pacf(y2, main = expression(paste("AR(1) com", phi, "= -0.8")), xlab = "defasagem", 
     ylab = "PFAC")

# Modelo AR(2)
y3 = arima.sim(model = list(order(2,0,0), ar = c(0.75, -0.5)), n = N)
y3

par(mfrow = c(3, 1))

plot.ts(y3, xlab = "tempo", ylab = "observações")
acf(y3, main = expression(paste("AR(2) com", phi, "= 0.75, -0.5")), xlab = "defasagem", 
    ylab = "FAC")
pacf(y2, main = expression(paste("AR(2) com", phi, "= 0.75, -0.5")), xlab = "defasagem", 
     ylab = "PFAC")

# Processo MA(1)

y4 = arima.sim(model = list(order(0,0,1), ma = 0.8), n = N)
par(mfrow = c(3,1))

plot.ts(y4, xlab = "tempo", ylab = "observações")
acf(y4, main = expression(paste("MA(1) com", theta, "= 0.8")), xlab = "defasagem", 
    ylab = "FAC")
pacf(y4, main = expression(paste("MA(1) com", theta, "= 0.8")), xlab = "defasagem", 
     ylab = "PFAC")

y5 = arima.sim(model = list(order(0,0,2), ma = c(-0.6, 0.8)), n = N)
par(mfrow = c(3,1))

plot.ts(y5, xlab = "tempo", ylab = "observações")
acf(y5, main = expression(paste("MA(2) com", theta, "= -0.6, 0.8")), xlab = "defasagem", 
    ylab = "FAC")
pacf(y5, main = expression(paste("MA(2) com", theta, "= -0.6, 0.8")), xlab = "defasagem", 
     ylab = "PFAC")

# Processo ARMA(1,1)

y6 = arima.sim(model = list(order(1,0,1), ar = 0.8, ma = 0.6), n = N)
par(mfrow = c(3,1))

plot.ts(y6, xlab = "tempo", ylab = "observações")
acf(y6, main = expression(paste("ARMA(1,1) com", phi, "= 0.8", theta, "= 0.6")), xlab = "defasagem", 
    ylab = "FAC")
pacf(y6, main = expression(paste("ARMA(1,1) com", phi, "0.8", theta, "= 0.6")), xlab = "defasagem", 
     ylab = "PFAC")

y7 = arima.sim(model = list(order(2,0,1), ar = c(0.8, -0.4), ma = 0.6), n = N)
par(mfrow = c(3,1))

# Processo ARMA(2,1)

plot.ts(y7, xlab = "tempo", ylab = "observações")
acf(y7, main = expression(paste("ARMA(2,1) com", phi, "= 0.8, -0.4", theta, "= 0.6")), xlab = "defasagem", 
    ylab = "FAC")
pacf(y7, main = expression(paste("ARMA(2,1) com", phi, "0.8, -0.4", theta, "= 0.6")), xlab = "defasagem", 
     ylab = "PFAC")
