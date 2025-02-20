#Aula - Heterocedasticidade

reg1=lm(PRICE ~LOTSIZE + SQRFT + BDRMS, data=hprice)
summary(reg1)

summary(hprice$LOTSIZE)
#Gr�ficos de dispers�o
par(mfrow=c(1,2))
plot(hprice$LOTSIZE,reg1$residuals)
plot(hprice$LOTSIZE,reg1$residuals^2)

plot(hprice$SQRFT,reg1$residuals)
plot(hprice$SQRFT,reg1$residuals^2)

plot(hprice$BDRMS,reg1$residuals)
plot(hprice$BDRMS,reg1$residuals^2)

#Teste de Breuch-Pagan
install.packages('lmtest')
library(lmtest)

y=fitted(reg1)
dados=cbind(y, hprice$PRICE)
View(dados)

bptest(reg1)

#Teste de White
bptest(reg1, ~fitted(reg1)+ fitted(reg1)^2)

#Teste BP Manualmente
aux=lm(residuals(reg1)^2 ~ LOTSIZE + SQRFT + BDRMS, data=hprice)
summary(aux)

install.packages('stargazer')
library(stargazer)

#Estat�sticas robustas
install.packages('car')
library(car)

setwd('R:\\HO-231-Rosangela\\Slides')

stargazer(coeftest(reg1),coeftest(reg1,vcov=hccm(reg1,type='hc0')),
          digits=8,column.labels = c('Usual','Robusta'),type='text',
          out='robusta.txt',data=hprice)

linearHypothesis(reg1,c('LOTSIZE','SQRFT'),vcov=hccm(reg1,type='hc0'))


#Modelo log
reg2=lm(log(PRICE) ~log(LOTSIZE) + log(SQRFT)
        + BDRMS, data=hprice)
summary(reg2)

bptest(reg2)
stargazer(coeftest(reg2),coeftest(reg2,vcov=hccm(reg2,type='hc0')),
          digits=8,column.labels = c('Usual','Robusta'),type='text',
          out='robusta2.txt',data=hprice)

#M�nimos Quadrados Generalizados
#Obter os res�duos
residuos=(reg1$residuals)^2

#Log dos res�duos
lresiduos=log(residuos)

#Regress�o do log(res) em fun��o das vari�veis independentes
auxiliar=lm(lresiduos ~ LOTSIZE + SQRFT + BDRMS, data=hprice)
summary(auxiliar)

#Valores estimados da regress�o auxiliar
g=auxiliar$fitted.values

#Fator de pondera��o
h=exp(g)

#Estimativas por MQP (MQG ou MQG fact�vel)
modeloMQG=lm(PRICE ~LOTSIZE + SQRFT + BDRMS,weights = 1/h,data=hprice)
stargazer(coeftest(reg1),coeftest(reg1,vcov=hccm(reg1,type='hc0')),
          coeftest(modeloMQG),
          coeftest(modeloMQG,vcov=hccm(modeloMQG,type='hc0')),
          digits=8,
          column.labels = c('MQO','Robusta','MQG-fact�vel',
                            'MQG Robusto'),
          type='text',out='saida.txt',data=hprice)


























































































