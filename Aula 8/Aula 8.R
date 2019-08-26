## Metodologia Box & Jenkins 

# Construção do modelo de séries temporais:

# 1. Identificação: com base na análise de autocorrelação,
# autocorrelação parcial e/ou critérios de informação;

# 2. Estimação: os parâmetros do modelo identificado são estimados;

# 3. Verificação do modelo ajustado: por meio de uma análise de
# resı́duos, averigua-se se este é adequado para os fins em vista,
# no caso, para a previsão.

# Packages importantes

library(forecast) # ajuste e previsão do modelo ARIMA
library(lmtest) # Test hipótese
library(FinTS) # Test de heterocedasticidade
library(urca) # Test de RU
library(tseries) # Test de normalidade 
library(TSA) # Test 

# Leitura dos dados
ipca = ts(IPCA2019[,2], start = c(1998, 1), frequency = 12)

# Identificação do Modelo
# Gráfico

par(mfrow = c(2, 1))

plot(ipca, main = "IPCA", ylab = "IPCA", xlab = "Meses")
acf(ipca, lag.max = 24, drop.lag.0 = TRUE)

# Test ADF
df1 = ur.df(ipca, type = "trend", lags = 12)
plot(df1)

# On peut encore ajouter BIC
df12 = ur.df(ipca, type = "trend", lags = 12, selectlags = "BIC")
plot(df12)
summary(df12)

df2 = ur.df(ipca, type = "drift", lags = 12, selectlags = "BIC")
plot(df2)
summary(df2) # Logo, a serie é estacionário, d = 0

# Identificação do type e da ordem do modelo (FACP ou PACF)
par(mfrow = c(1, 2))
acf(ipca, type = "correlation", lag.max = 24, drop.lag.0 = TRUE)
acf(ipca, type = "partial", lag.max = 24, drop.lag.0 = TRUE)

# Estimação do modelos

# Modelo ARIMA (1, 0, 0)

modelo1 = Arima(ipca, order = c(1, 0, 0), method = "ML")
summary(modelo1)

# Modelo ARIMA (1, 0, 1)

modelo2 = Arima(ipca, order = c(1, 0, 1), method = "ML")
summary(modelo2)

# Verificação dos modelos

# Significancia estatistica
fit1 = coeftest(modelo1)
print(fit1) # coeficientes sao significativos

fit2 = coeftest(modelo2)
print(fit2) # MA (1) não é significativo

# Autocorrelação dos resíduos
par(mfrow = c(2, 1))
acf(modelo1$residuals, drop.lag.0 = T)
acf(modelo2$residuals, drop.lag.0 = T)

# Test de Box-Pierce
Box.test(modelo1$residuals, lag = 4, type = "Box-Pierce",
         fitdf = 1) # fitdf = 1 càd degré de liberté, dans notre 
                    # modele c'est 1, puisque nous avons AR(1).
Box.test(modelo1$residuals, lag = 8, type = "Box-Pierce", 
         fitdf = 1)

Box.test(modelo1$residuals, lag = 12, type = "Box-Pierce", 
         fitdf = 1)

# Test de Ljung-Box
Box.test(modelo1$residuals, lag = 4, type = "Ljung-Box", 
         fitdf = 1)
Box.test(modelo1$residuals, lag = 8, type = "Ljung-Box", 
         fitdf = 1)
Box.test(modelo1$residuals, lag = 12, type = "Ljung-Box", 
         fitdf = 1)

# Gráfico dos p-valores com Ljung-Box
tsdiag(modelo1, gof.lag = 20)

# Test de Ljung-Box
Box.test(modelo2$residuals, lag = 4, type = "Ljung-Box", 
         fitdf = 2)
Box.test(modelo2$residuals, lag = 8, type = "Ljung-Box", 
         fitdf = 2)
Box.test(modelo2$residuals, lag = 12, type = "Ljung-Box", 
         fitdf = 2)

#Gráfico de Ljung-Box
tsdiag(modelo2, gof.lag = 20)

# Test de normalidade
par(mfrow = c(1, 2))
hist(modelo1$residuals)
plot(density(modelo1$residuals, kernel = "gaussian"))

jarque.bera.test(modelo1$residuals)
shapiro.test(modelo1$residuals)

par(mfrow = c(1, 2))
hist(modelo2$residuals)
plot(density(modelo2$residuals, kernel = "gaussian"))

jarque.bera.test(modelo2$residuals)
shapiro.test(modelo2$residuals)

# Test de heterocedasticidade condicional
ArchTest(modelo1$residuals, lag = 4)
ArchTest(modelo1$residuals, lag = 8)
ArchTest(modelo1$residuals, lag = 12)

ArchTest(modelo2$residuals, lag = 4)
ArchTest(modelo2$residuals, lag = 8)
ArchTest(modelo2$residuals, lag = 12)

# Aplicar Transformação Box-Cox (1964)
lambda = BoxCox.lambda(ipca)
print(lambda)

# Modelo ARIMA (1, 0, 0)

modelo3 = Arima(ipca, order = c(1, 0, 0), method = "ML", 
                lambda = lambda)
summary(modelo3)

# Modelo ARIMA (1, 0, 1)

modelo4 = Arima(ipca, order = c(1, 0, 1), method = "ML", 
                lambda = lambda)
summary(modelo4)

#Verificação dos modelos
fit3 = coeftest(modelo3)
fit4 = coeftest(modelo4)
print(fit3)
print(fit4)

# Gráfico dos p-valores com Ljung-Box
tsdiag(modelo3, gof.lag = 20) # resolveu o problema da normalidade
tsdiag(modelo4, gof.lag = 20) # Idem

# Test de normalidade
par(mfrow = c(1, 2))
hist(modelo3$residuals)
plot(density(modelo3$residuals, kernel = "gaussian"))

jarque.bera.test(modelo3$residuals)
shapiro.test(modelo3$residuals)

par(mfrow = c(1, 2))
hist(modelo4$residuals)
plot(density(modelo4$residuals, kernel = "gaussian"))

jarque.bera.test(modelo4$residuals)
shapiro.test(modelo4$residuals)

# Test de heterocedasticidade condicional
ArchTest(modelo3$residuals, lag = 4)
ArchTest(modelo3$residuals, lag = 8)
ArchTest(modelo3$residuals, lag = 12)

ArchTest(modelo4$residuals, lag = 4)
ArchTest(modelo4$residuals, lag = 8)
ArchTest(modelo4$residuals, lag = 12)

AIC(modelo3, modelo4)
BIC(modelo3, modelo4) # modele 3 est meilleure puisque AIC e BIC
                      # sont inférieurs.

# Previsão
prev = forecast(modelo3, h = 6, level = c(0.90, 0.95), 
                lambda = lambda, biasadj = F)
print(prev)

par(mfrow = c(1, 1))
plot(prev)
summary(prev)
