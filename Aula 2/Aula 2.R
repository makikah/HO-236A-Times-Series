## Decomposição da ST

pib = ts(PIB_Agropecuaria[,2], start = c(1996, 1), frequency = 4)
plot(pib)
lpib = log(pib)
par(new = TRUE)
plot(lpib, col = "blue")

library("gets")

d = periodicdummies(pib)
View(d)

trend = ts(seq(1, length(pib)), frequency = 4)
View(trend)

model = lm(pib ~ trend + d[,1:3])
summary(model)

model2 = lm(lpib ~ trend + d[,1:3])
summary(model2)

tendencia = ts(model2$coefficients[1] + model2$coefficients[2]*trend)
sazonal = ts(model2$coefficients[3]*d$dum1 + model2$coefficients[4]*d$dum2 + 
               model2$coefficients[5]*d$dum3)
residuo = ts(resid(model2))

par(mfrow = c(2, 2))
plot(lpib, ylab = "log(PIB", xlab = "Tempo", main = "Serie PIB")
plot(tendencia, ylab = "Trend")
plot(sazonal, ylab = "Sazonal")
plot(residuo)

## Modelo de previsão
model3 = lm(lpib[1:88] ~ trend[1:88] + d[1:88, 1:3])
summary(model3)

saz = ts(d)
b = model3$coefficients
b

prev = matrix(NA, nrow = 4, ncol = 1)
erro = matrix(NA, nrow = 4, ncol = 1)
errop = matrix(NA, nrow = 4, ncol = 1)

for (i in 1:4) {
  prev[i] = b[1] + b[2]*(i + 88) + b[3]*saz[i + 88, 1] + b[4]*saz[i + 88, 2] + b[5]*saz[i + 88, 3]
  erro[i] = lpib[i + 88] - prev[i]
  errop[i] = lpib[i + 88] - prev[i]/lpib[i + 88]
}

reqm = sqrt(sum(erro^2)/length(erro))
eam = sum(abs(erro))/length(erro)
epm = 100*(sum(abs(errop))/length(erro))

n = length(lpib)
par(mfrow = c(1,1))
plot.ts(lpib[89:n], xlab = "Tempo", ylab = "Log(PIB)")
par(new = TRUE)
plot.ts(prev, axes = F, ann = F, col = "blue")

## Decomposição por Média Móvel (Moyenne Mobile)

model4 = decompose(pib, type = "additive")
plot(model4)

trend_mod4 = model4$trend
plot(trend_mod4)

model5 = decompose(pib, type = "multiplicative")
plot(model5)

trend_mod5 = model5$trend
plot.ts(trend_mod5)

## Aplicação do Filtro Hondrick-Prescott
install.packages("mFilter")
library("mFilter")

filtro_hp = hpfilter(pib, type = "lambda")
par(mfrow = c(2,1))

plot(pib, ylab = "PIB Agropecuaria")
lines(filtro_hp$trend, col = "red")
plot(filtro_hp$cycle, ylab = "Comp Cíclico")
lines(filtro_hp$cycle, col = "blue")

## Método de Suavização exponencial simples
set.seed(1234)
serie = ts(runif(100, 10, 15), frequency = 1)

par(mfrow = c(1, 1))
plot(serie)

ajuste = HoltWinters(serie, beta = FALSE, gamma = F)
ajuste
plot(ajuste)

# Ou
ajuste = HoltWinters(serie, alpha = 0.96, beta = FALSE, gamma = F)
ajuste
plot(ajuste)

## Previsão
ajust_prev = predict(ajuste, n.ahead = 10, prediction.interval = TRUE, level = 0.95)
ajust_prev
plot(ajuste, ajust_prev)


## Modelo Suavização de Holt
consumo = ts(ConsumoEnergia[,2], start = c(1979, 1), frequency = 12)
plot(consumo)

ajust_holt = HoltWinters(consumo, gamma = F)
ajust_holt
plot(ajust_holt)
fitted(ajust_holt)
prev_holt = predict(ajust_holt, n.ahead = 12, prediction.interval = TRUE)
plot(ajust_holt, prev_holt)

## Modelo HW
desemprego = ts(TaxaDesemprego[,2], start = c(1984, 12), frequency = 1)
desemprego = desemprego[-1]
View(desemprego)
desemp = ts(desemprego, start = c(1985, 1), frequency = 1)
plot(desemp)

ajus_hw = HoltWinters(desemp, gamma = F)
plot(desemp, ylab = "Valores")
lines(fitted(ajus_hw)[,1], col = "red")

prev_hw = predict(ajus_hw, n.ahead = 12)
plot(ajus_hw, prev_hw)
