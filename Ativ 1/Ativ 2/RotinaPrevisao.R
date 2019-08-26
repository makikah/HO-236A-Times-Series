
#Avaliar as previsões
#Modelo 1
erro1 <- matrix(NA, nrow=length(window(ibc, start=2016)), ncol=1)
errop<-matrix(NA, nrow=length(window(ibc, start=2016)), ncol=1)

real <- matrix(NA, nrow=length(window(ibc, start=2016)), ncol=1)
previsto<- matrix(NA, nrow=length(window(ibc, start=2016)), ncol=1)

for (i in 1:length(erro1)){
  previsto[i]=predict(arima(ibc[1:(length(window(ibc, end=c(2015, 12)))+i-1)], order=c(0,1,2)),n.ahead=1)$pred
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

legend('topright', c('IBC', 'Previsto'), col=c('blue', 'red'), lty=1:2, bty='n')
grid()
