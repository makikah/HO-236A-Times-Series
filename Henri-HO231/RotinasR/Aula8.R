#Aula de Problema de Especificação

install.packages('car')
library(car)

install.packages('normtest')
library(normtest)

install.packages('stargazer')
library(stargazer)

#Gráfico de dispersão
pairs(hprice[,2:5],panel=panel.smooth)

#Análise Descritiva dos dados
summary(hprice[2:5])
cor(hprice[,2:5])

#Modelo Linear
reg1 = lm(PRICE ~LOTSIZE + SQRFT + BDRMS, data=hprice)
summary(reg1)

vif(reg1)

#Modelo Quadrático
reg2=lm(PRICE~LOTSIZE+SQRFT+BDRMS+I(LOTSIZE^2)+I(SQRFT^2), data=hprice)
summary(reg2)

#teste de restrição
linearHypothesis(reg2,c('I(LOTSIZE^2)=0', 'I(SQRFT^2)=0'),test='F')

#Tabela de resultados
getwd()
setwd('R:\\HO-231-Rosangela\\Dados')

stargazer(reg1,reg2,digits=8,type='text',
          column.labels=c('Linear','Quadrático'),
          keep.stat=c('n','rsq','adj.rsq','f'),out='ModeloPrice.txt')


#Teste de Reset
reg3=lm(PRICE ~LOTSIZE+SQRFT+BDRMS+
          I(fitted(reg1)^2)+I(fitted(reg1)^3),data=hprice)

linearHypothesis(reg3,
                 c('I(fitted(reg1)^2)=0','I(fitted(reg1)^3)=0'),test='F')

#Teste Reset Automático no R
install.packages('lmtest')
library(lmtest)

resettest(reg1)

reg4=lm(PRICE ~LOTSIZE+SQRFT+BDRMS+I(LOTSIZE^2),data=hprice)
summary(reg4)
resettest(reg4)

#Teste de Davidson
reg5=lm(PRICE~log(LOTSIZE)+log(SQRFT)+BDRMS,data=hprice)

reg6=lm(PRICE~LOTSIZE+ SQRFT +BDRMS+ I(fitted(reg5)),data=hprice)
summary(reg6)

stargazer(reg1,reg5,digits=8,type='text',
          column.labels=c('Linear','Log'),
          keep.stat=c('n','rsq','adj.rsq','f'),out='ModeloPrice1.txt')

#Critério AIC e SIC (=BIC)
AIC(reg1,reg5)
BIC(reg1,reg5)

resettest(reg5)



















