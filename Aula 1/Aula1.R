## Decomposição de séries Times

par(mfrow = c(2,2))

## PIB Agropecuária
pib_agro = ts(PIB_Agropecuaria[,2], start = c(1996, 1), frequency = 4)
plot(pib_agro, main = "PIB Agropecuaria", xlab = "Trim/Ano", ylab = "PIB")
grid()

##PIB Alemanha
pib_alemanha = ts(PIB_Alemanha[,2], start = 1960, frequency = 1)
plot(pib_alemanha, main = "PIB Alemanha", xlab = "Ano", ylab = "PIB")
grid()

##Vazões Furnas
vazoes = ts(VazoesFurnas, start = c(1999, 1), frequency = 12)
plot(vazoes, main = "Vazoes", xlab = "Mes/Ano", ylab = "Vazoes m3/s")
grid()

## Taxa de Cambio
cambio = ts(TaxaCambio[,2], start = c(2000, 1), frequency = 12)
plot(cambio, main = "Taxa de Cambio", xlab = "Trim/Ano", ylab = "Taxa")
grid()

## Test de F pour vérifier la saisonalité. si F = 0 pas de saisonalité, mais si F=1 présence de saisonnalité.

## Remoção da sazonalidade - Vazões Furnas
install.packages("gets")
library(gets)

## Criação de binária
d = periodicdummies(vazoes)
View(d)

## Estimação do componente sazonal por MQO
vaz.reg = lm(vazoes ~ d[,2:12])
summary(vaz.reg)

vaz.des = ts(resid(vaz.reg), start = c(1999, 1), freq = 12)
vaz.saz = ts(fitted(vaz.reg), start = c(1999, 1), frequency = 12)

par(mfrow = c(2, 2))
plot(vaz.des)
grid()
plot(vaz.saz)
grid()

## Gráfico da série original e as demais

par(mfrow = c(1,1))
plot(vazoes, col = "blue")

par(new = TRUE)
plot(vaz.des, col = "red")

par(new = TRUE)
plot(vaz.saz, col = "green")

## Remover tendencia do PIB Alemanha
trend = ts(seq(1, length(pib_alemanha)), frequency = 1)
View(trend)

## Remover a tendencia
alemanha.trend = lm(pib_alemanha ~ trend)
summary(alemanha.trend)

alemanha.hat = ts(fitted(alemanha.trend), start = 1960, frequency = 1)
View(alemanha.hat)
alemanha.res = ts(resid(alemanha.trend), start = 1960, frequency = 1)

## Gráfico da série original 

par(mfrow = c(1,2))
plot(alemanha.hat)
plot(alemanha.res)

par(mfrow = c(1,1))
plot(pib_alemanha, col = "blue")

par(new = TRUE)
plot(alemanha.hat, col = "red")

par(new = TRUE)
plot(alemanha.res, col = "green")
