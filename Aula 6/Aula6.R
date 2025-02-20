N=100

set.seed(123)
aleat=ts(rnorm(N,0,1))

y1=ts(rep(0,N))
y2=2

D1=ts(rep(0,N))
D2=ts(rep(0,N))
for (t in 1:N){
  if (t>40)
    D1[t]=3
  if (t==41)
    D2[t]=3
}

for (t in 2:N){
  y1[t]=0.5*y1[t-1]+aleat[t]+D1[t]
  y2[t]=y2[t-1]+aleat[t]+D2[t]
}

par(mfrow=c(1,2))
plot(y1)
plot.ts(y2)

#Teste de Quebra Estrutural
install.packages('strucchange')
library(strucchange)

cambio=ts(CambioComercial[,2],start=c(1995,1),freq=12)
par(mfrow=c(1,1))
plot(cambio)

#Teste de Chow/Zeileis et al (2001)
cambio.chow=Fstats(cambio ~ 1,from=0.15)
breakpoints(cambio.chow)

cambio1=ts(CambioComercial[,2],start=c(1995,1),end=c(2008,6),freq=12)
par(mfrow=c(1,1))
plot(cambio1)

cambio1.chow=Fstats(cambio1 ~ 1,from=0.15)
breakpoints(cambio1.chow)
sctest(cambio1.chow)
lines(breakpoints(cambio1.chow))

#teste de Bai e Perron
cambio1.bp=breakpoints(cambio1~1,h=0.15,breaks=NULL)
summary(cambio1.bp)
plot(cambio1.bp)

#Teste de Perron
#Teste de quebra estrutural ex�gena
N=length(cambio1)

DP=ts(rep(0,N))
DL=ts(rep(0,N))
DS=ts(rep(0,N))
Tb=49

for (t in 1:N){
  if (t==Tb+1)
    DP[t]=1
  if (t>Tb){
    DL[t]=1
    DS[t]=t-Tb
  }
}
View(DP)
View(DL)
View(DS)

#Ajuste do modelo da hip�tese alternativa- Escolha do modelo A
trend=ts(seq(1,N))
modelo = lm (cambio1 ~ trend + DL)
summary(modelo)

#Obter os res�duos
res=modelo$residuals
plot.ts(res)

install.packages('urca')
library(urca)

#Teste ADF nos res�duos
res.df=ur.df(res,type='none',lags=6,selectlags = 'BIC')
summary(res.df)
plot(res.df)

res1.df=ur.df(res,type='none',lags=0)
summary(res1.df)
plot(res1.df)

lambda=Tb/N
lambda

#Teste de Zivot e Andrews - quebra end�gena
cambio1.za=ur.za(cambio1,model='intercept')
summary(cambio1.za)
plot(cambio1.za)

cambio1.za2=ur.za(cambio1,model='trend')
summary(cambio1.za2)
plot(cambio1.za2)

cambio1.za3=ur.za(cambio1,model='both')
summary(cambio1.za3)
plot(cambio1.za3)












