#Aula VECM e Teste de Cointegra��o Johansen
#Exemplo Dados Setor P�blico

#Instala��o de pacotes
install.packages('MASS') #Matrizes
install.packages('lmtest') #Testes LM
install.packages('urca') #Teste de RU
install.packages('MTS') #An�lise de Modelos VARMA
install.packages('vars') #Modelos VAR

#Carregando pacotes
library(MASS)
library(lmtest)
library(urca)
library(MTS)
library(vars)
require(graphics)
library(tseries)



Dados=ts(DadosSetorPublico[,2:5],start=c(2001,12),freq=12)
View(Dados)

nfsp=Dados[,1]
dlsp=Dados[,2]
selic=Dados[,3]
ipca=Dados[,4]

juros=selic-ipca

#Gr�fico das s�ries
par(mfrow=c(2,2))
plot(nfsp,main='NFSP',xlab='',ylab='NFSP')
plot(dlsp,main='DLSP',xlab='',ylab='DLSP')
plot(juros,main='Juros Real',xlab='',ylab='Juros')
plot(ipca,main='IPCA',xlab='',ylab='IPCA')

#Gr�fico das s�ries em diferen�a
par(mfrow=c(2,2))
plot(diff(nfsp),main='NFSP',xlab='',ylab='NFSP')
plot(diff(dlsp),main='DLSP',xlab='',ylab='DLSP')
plot(diff(juros),main='Juros Real',xlab='',ylab='Juros')
plot(diff(ipca),main='IPCA',xlab='',ylab='IPCA')

#Tomou o log
lnfsp=log((nfsp/100)+1)
ldlsp=log(dlsp)
ljuros=log(juros)
lipca=log(ipca)

par(mfrow=c(2,2))
plot(lnfsp,main='NFSP',xlab='',ylab='log(NFSP)')
plot(ldlsp,main='DLSP',xlab='',ylab='log(DLSP)')
plot(ljuros,main='Juros Real',xlab='',ylab='log(Selic)')
plot(lipca,main='IPCA',xlab='',ylab='log(IPCA)')

#Gr�fico das s�ries em diferen�a
par(mfrow=c(2,2))
plot(diff(lnfsp),main='NFSP',xlab='',ylab='NFSP')
plot(diff(ldlsp),main='DLSP',xlab='',ylab='DLSP')
plot(diff(ljuros),main='Juros Real',xlab='',ylab='Juros')
plot(diff(lipca),main='IPCA',xlab='',ylab='IPCA')


Ldados=data.frame(ljuros,ldlsp,lnfsp,lipca)

#Teste de raiz Unit�ria
lipca.trend=ur.df(lipca, type=c("trend"),lags=12, selectlags = "BIC")
summary(lipca.trend)
plot(lipca.trend)

lipca.drift=ur.df(lipca, type=c("drift"),lags=12, selectlags = "BIC")
summary(lipca.drift)
plot(lipca.drift)

lipca.none=ur.df(lipca, type=c("none"),lags=12, selectlags = "BIC")
summary(lipca.none)
plot(lipca.none)

#Fazer teste de RU para as demais s�ries

#Teste de Cointegra��o
#Teste de PO
X=data.frame(ljuros,ldlsp)
cointest_po<-ca.po(X,demean="none",lag="short",type='Pu')
summary(cointest_po)

#Teste de Johansen

#Ordem do VAR
#Sele��o da Ordem do VAR
m2=VARselect(Ldados,lag.max=12,type='const')
m2

model.coint=ca.jo(Ldados,type='eigen', K=2,ecdet='const')
summary(model.coint)

model.coint=ca.jo(Ldados,type='trace', K=2,ecdet='const')
summary(model.coint)


#Estima��o do Modelo
modelo.vecm<-cajorls(model.coint,r=1)

#Apresenta o vetor de cointegra��o
modelo.vecm$beta

#Apresenta cada uma das equa��es
summary(modelo.vecm$rlm)

#An�lise de Estabilidade do Modelo
modelo.var=vars::VAR(diffM(Ldados),p=1,type='const')
summary(modelo.var)

#An�lise das ra�zes - an�lise da estabilidade  das ra�zes do sistema
roots(modelo.var)



#Res�duos dos Modelos
acf(residuals(modelo.vecm)[,1])
acf(residuals(modelo.vecm)[,2])
acf(residuals(modelo.vecm)[,3])
acf(residuals(modelo.vecm)[,4])

#Teste e Portmanteau
#A partir daqui os resultados s�o distintos aos obtidos pelo Eviews
model.pt.asy=serial.test(modelo.vecm,lags.pt=8, type='PT.asymptotic')
model.pt.asy


model.pt.adj=serial.test(modelo.vecm,lags.pt=9, type='PT.adjusted')
model.pt.adj

model.BG=serial.test(modelo.vecm,lags.bg=10,type='BG')
model.BG

normality.test(modelo.vecm, multivariate.only=FALSE)

