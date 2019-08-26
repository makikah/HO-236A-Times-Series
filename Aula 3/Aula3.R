## Processos Estocásticos

## Tamanho da amostra
N = 200

##item a)
set.seed(123)
y1 = ts(rnorm(N, 0, 1))
par(mfrow = c(2, 1))
plot(y1, xlab = "Tempo", ylab = "obervações")
acf(y1, main = "FAC", ylab = "Autocorélation", xlab = "Defasagens")

##item b)
y2 = ts(rep(0, N))

for (i in 1:N) {
  y2[i] = 0.5 + 0.1*i + y1[i]
}

plot(y2, xlab = "Tempo", ylab = "obervações")
acf(y2, main = "FAC", ylab = "Autocorélation", xlab = "Defasagens")


##item c)
set.seed(123)
epsilon = ts(rnorm(N, 0, 5))

y3 = ts(cumsum(epsilon))

plot(y3, xlab = "Tempo", ylab = "obervações")
acf(y3, main = "FAC", ylab = "Autocorélation", xlab = "Defasagens")

##item d)
aleatorio = ts(y1)

y4 = 0.0
for (i in 2:N) {
  y4[i] = 0.7*y4[i-1] + aleatorio[i]
}
y4 = ts(y4)
plot(y4, xlab = "Tempo", ylab = "obervações")
acf(y4, main = "FAC", ylab = "Autocorélation", xlab = "Defasagens")

## AR(1) se vérifie uniquement dans un modèle stationnaire.

##item e)
y5 = 0.0

for (i in 2:N) {
  y5[i] = -0.8*y5[i-1] + aleatorio[i]
}

y5 = ts(y5)

plot(y5, xlab = "Tempo", ylab = "obervações")
acf(y5, main = "FAC", ylab = "Autocorélation", xlab = "Defasagens")

##item f)
y6 = ts(cumsum(aleatorio + 1))

plot(y6, xlab = "Tempo", ylab = "obervações")
acf(y6, main = "FAC", ylab = "Autocorélation", xlab = "Defasagens")

##item g)
y7 = 0.0

for (i in 2:N) {
  y7[i] = 3.0 + 0.5*i + y7[i-1] + aleatorio[i]
}

y7 = ts(y7)

plot(y7, xlab = "Tempo", ylab = "obervações")
acf(y7, main = "FAC", ylab = "Autocorélation", xlab = "Defasagens")

##Gráfico series temporais e FAC

##PIB Agropecuária
agro = ts(PIB_Agropecuaria[,2], start = c(2000, 1), frequency = 4)
plot(agro, main = "", ylab = "PIB", xlab = "Anos")
acf(agro, main = "", ylab = "Autocorrelation", xlab = "Lags")

## Vazões
vaz = ts(VazoesFurnas, start = c(1999, 1), frequency = 12)
plot(vaz, main = "", ylab = "Vazoes", xlab = "Anos")
acf(vaz, main = "", ylab = "Autocorrelation", xlab = "Lags")

##Taxa de cambio
cambio = ts(TaxaCambio, start = c(2000, 1), frequency = 12)
plot(cambio, main = "", ylab = "Cambio", xlab = "Anos")
acf(cambio, main = "", ylab = "Autocorrelation", xlab = "Lags")

##PIB Alemanhã
vaz = ts(VazoesFurnas, start = c(1999, 1), frequency = 12)
plot(vaz, main = "", ylab = "Vazoes", xlab = "Anos")
acf(vaz, main = "", ylab = "Autocorrelation", xlab = "Lags")

##Taxa de desemprego
vaz = ts(VazoesFurnas, start = c(1999, 1), frequency = 12)
plot(vaz, main = "", ylab = "Vazoes", xlab = "Anos")
acf(vaz, main = "", ylab = "Autocorrelation", xlab = "Lags")

##Consumo Energia
consumo = ts(VazoesFurnas, start = c(1999, 1), frequency = 12)
plot(vaz, main = "", ylab = "Vazoes", xlab = "Anos")
acf(vaz, main = "", ylab = "Autocorrelation", xlab = "Lags")