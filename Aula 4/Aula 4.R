## Teste de raiz unitária

install.packages("TSA")
library(TSA)

cambio = ts(Cambio[,2], start = c(1999, 1), frequency = 12)

par(mfrow = c(2, 2))

plot(cambio, main = "Taxa de câmbio", ylab = "Cambio")
acf(cambio, ylab = "FAC", xlab = "Lags")

dcambio = diff(cambio)
plot(dcambio, main = "Diferença do cambio")
acf(dcambio, ylab = "FAC", xlab = "Lags")

## Test de ADF

library(urca)

df1 = ur.df(cambio, type = "trend", lags = 0)
summary(df1)
plot(df1)

cam.df1 = ur.df(cambio, type = "trend", lags = 1)
summary(cam.df1)
plot(cam.df1)

## Akaike
akaike.df1 = ur.df(cambio, type = "trend", lags = 12, selectlags = "AIC")
summary(akaike.df1)
plot(akaike.df1)

## BIC
bic.df1 = ur.df(cambio, type = "trend", lags = 12, selectlags = "BIC")
summary(bic.df1)
plot(bic.df1)

bic.df2 = ur.df(cambio, type = "drift", lags = 12, selectlags = "BIC")
summary(bic.df2)
plot(bic.df2)

bic.df3 = ur.df(cambio, type = "none", lags = 12, selectlags = "BIC")
summary(bic.df3)
plot(bic.df3) 
## Não rejeita H0 pois cambio segue uma raiz unitária. Logo, a variação é stacionária.

## Teste para a diferença do cambio
dcam.df = ur.df(dcambio, type = "drift", lags = 12, selectlags = "BIC")
summary(dcam.df)
plot(dcam.df)
## Notre serie ne precise pas l'intercept, alors:

dcam.df0 = ur.df(dcambio, type = "none", lags = 12, selectlags = "BIC")
summary(dcam.df0)
plot(dcam.df0)
