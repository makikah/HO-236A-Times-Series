#Multicolineariedade

dados = Dados_Co2PibPop[,4:7]

modelo=lm(log(co2) ~ log(pib) + log(pop), data=dados)
summary(modelo)

install.packages('car')
library(car)

vif(modelo)

#Modelo de regressão auxiliar
aux=lm(log(pib)~log(pop),data=dados)
summary(aux)

aux1=lm(log(pop)~log(pib),data=dados)
summary(aux1)

#Transformação de variáveis
modelo2=lm(log(co2/pop) ~log(pib/pop), data=dados)
summary(modelo2)


















