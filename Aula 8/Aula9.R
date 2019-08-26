#Metodologia de Box & Jenkins
#Série IBC-BR

#Instalar os Pacotes
install.packages('forecast')
install.packages('FinTS')
install.packages('urca')
install.packages('TSA')
install.packages('tseries')
install.packages('lmtest')

#Carregar pacotes
library(forecast)
library(FinTS)
library(urca)
library(TSA)
library(tseries)
library(lmtest)

#Leitura e análise dos dados
ibc=ts(IBC_BR[,2],start=c(2003,1),freq=12)

#Gráfico da série
layout(1:2) #par(mfrow=c(2:1))

plot(ibc,main='Índice de Atividade Econômica',
     xlab='Mês/Ano',ylab='IBC-BR')
acf(ibc,lag.max = 36,drop.lag.0 = T,type='correlation',
    xlab='lags',ylab='FAC')

#Teste de Raiz Unitária
#Teste de Zivot-Andrews
za.ibc=ur.za(ibc,model='trend')
summary(za.ibc)
plot(za.ibc)

##Teste ADF
#Modelo com intercepto e tendência
#DF
df.ibc=ur.df(ibc,type='trend',lags=0)
plot(df.ibc)

adf.ibc=ur.df(ibc,type='trend',lags=12,selectlags='BIC')
plot(adf.ibc)
summary(adf.ibc)

adf.ibc2=ur.df(ibc,type='drift',lags=12,selectlags='BIC')
plot(adf.ibc2)
summary(adf.ibc2)

adf.ibc3=ur.df(ibc,type='none',lags=12,selectlags='BIC')
plot(adf.ibc3)
summary(adf.ibc3)

#Teste PP
pp.ibc1=ur.pp(ibc,type='Z-tau',model='trend',lags='short')
plot(pp.ibc1)
summary(pp.ibc1)

pp.ibc2=ur.pp(ibc,type='Z-tau',model='constant',lags='short')
plot(pp.ibc2)
summary(pp.ibc2)

#Teste ERS
ers.ibc1=ur.ers(ibc,type='DF-GLS',model='trend',lag.max=2)
plot(ers.ibc1)
summary(ers.ibc1)

ers.ibc2=ur.ers(ibc,type='DF-GLS',model='constant',lag.max=2)
plot(ers.ibc2)
summary(ers.ibc2)

#Teste KPSS
kpss.ibc1=ur.kpss(ibc,type='tau',lags='short')
plot(kpss.ibc1)
summary(kpss.ibc1)

kpss.ibc2=ur.kpss(ibc,type='mu',lags='short')
plot(kpss.ibc2)
summary(kpss.ibc2)

#Conclusão: IBC-BR não estacionário

layout(1:1)
plot(diff(ibc))

#Ajuste ARIMA
#Dado que ibc é não estacionário, 
#devemos usar primeira diferença
layout(1:2)
acf(diff(ibc),drop.lag.0 = T,type='correlation')
acf(diff(ibc),type='partial')

#Estimação
mod1 = Arima(ibc,order=c(2,1,0),lambda=0)
mod2=Arima(ibc,order=c(0,1,2))
mod3=Arima(ibc,order=c(2,1,2))
mod4=Arima(ibc,order=c(1,1,1))

#Verificação
coeftest(mod1)
coeftest(mod2)
coeftest(mod3)
coeftest(mod4)

mod1
mod2
mod3
mod4

#Adequação dos resíduos
#Análise das correlações
acf(mod1$residuals,drop.lag.0 = T)
Box.test(mod1$residuals,lag=14,type='Ljung-Box')

for (i in 1:14){
  b=Box.test(mod1$residuals,i,type='Ljung-Box')$p.value
  print(b)
}

tsdiag(mod1,gof.lag=14)

#Teste de Normalidade
par(mfrow=c(1:2))
hist(mod1$residuals)
plot(density(mod1$residuals,kernel='gaussian'))

jarque.bera.test(mod1$residuals)
shapiro.test(mod1$residuals)

#Teste de Heterocedasticidade
ArchTest(mod1$residuals,lag=4)
FinTS::ArchTest(mod1$residuals,lag=8)

for (i in 1:14){
  b=ArchTest(mod1$residuals,i)$p.value
  print(b)
}

#Avaliar as previsões
#Modelo 1
erro1 <- matrix(NA, nrow=length(window(ibc, start=2016)), ncol=1)
errop<-matrix(NA, nrow=length(window(ibc, start=2016)), ncol=1)

real <- matrix(NA, nrow=length(window(ibc, start=2016)), ncol=1)
previsto<- matrix(NA, nrow=length(window(ibc, start=2016)), ncol=1)

lambda1=0
y=BoxCox(ibc,lambda=lambda1)

for (i in 1:length(erro1)){
  previsto[i]=InvBoxCox(predict(arima(y[1:(length(window(ibc, end=c(2015, 12)))+i-1)], order=c(2,1,0)),n.ahead=1)$pred,lambda=lambda1)
  real[i]=ts(ibc[(length(window(ibc, end=c(2015, 12)))+i)],start=2016, freq=12)
  previsto=ts(previsto,start=2016,freq=12)
  erro1[i]=real[i]-previsto[i]
  errop[i]=(real[i]-previsto[i])/real[i]
}


reqm1 <- sqrt(sum(erro1^2)/length(erro1))
reqm1
eam1 <- sum(abs(erro1))/length(erro1)
eam1
mape<-100*(sum(errop)/length(errop))
mape
par(mfrow=c(1,1))
plot(real, xlab='Ano', ylab='IBC',col='blue',lty=3)
lines(real,lty=3)
par(new=TRUE)
plot(previsto, axes=F,ann=F, col='red',lty=2)
legend('center', c('IBC', 'Previsto'), col=c('blue', 'red'), lty=1:2, bty='n')
grid()

#Previsões
prev= forecast(mod4,h=4,level=c(0.80, 0.95),lambda=lambda1,biasadj = F)
prev
plot(prev)



