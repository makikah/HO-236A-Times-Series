#Exemplo Emprego e Salário em Porto Rico

dados=ts(PRMINWGE,start=1950,freq=1)

trend=ts(1:nrow(dados))

reg = lm(log(PREPOP)~log(MINCOV)+ log(USGNP)+log(PRGNP)+trend,data=dados)
summary(reg)

#Análise dos resíduos
res1=resid(reg)

n=nrow(dados)
layout(1:2)
plot.ts(res1)
plot(res1[1:(n-1)],res1[2:n])

#Teste de Homocedasticidade
bptest(reg)

library(lmtest)

install.packages('car')
library(car)

coeftest(reg,vcov= hccm(reg,type='hc0'))

library(stargazer)

stargazer(coeftest(reg),coeftest(reg,vcov= hccm(reg,type='hc0')),
          digits=5,column.labels=c('Usual','Robusto'),type='text')

#teste de Correlação de Breuch-Godfrey
acf(res1)
pacf(res1)
bgtest(reg,order=1)

#Estatísticas robustas
install.packages('sandwich')
library(sandwich)

stargazer(coeftest(reg),coeftest(reg,vcovHAC),
          digits=5,column.labels=c('Usual','Robusto'),type='text')


#Correção de CO
install.packages('orcutt')
library(orcutt)

coch=cochrane.orcutt(reg)
coch
summary(coch)

#Correção PW
install.packages('prais')
library(prais)

pw=prais_winsten(reg)
summary(pw)


















