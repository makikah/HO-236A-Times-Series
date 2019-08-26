#Aula 2 - Ajuste modelo de regressão
dados=Dados_CO2[,2:4]

#modelo=lm(Dados_CO2$co2~Dados_CO2$pib+Dados_CO2$setor2)

modelo1 = lm(co2 ~ pib + setor2,data=dados)
coef(modelo1)

cor(dados$pib, dados$setor2)

pib1000= (dados$pib)/1000

modelo2 = lm(co2 ~ pib1000 + setor2,data=dados)
coef(modelo2)

modelo3=lm(co2 ~ pib1000 + I(pib1000^2)+setor2,data=dados)
coef(modelo3)

X=-(modelo3$coef[2])/(2*modelo3$coef[3])
X


#Omissão de Variáveis
modelo4 = lm(co2 ~ pib1000,data=dados)
coef(modelo4)

modelo2 = lm(co2 ~ pib1000 + setor2,data=dados)
coef(modelo2)

y=cor(pib1000,dados$setor2)
vies=y*modelo2$coef[3]
vies

summary(modelo3)









