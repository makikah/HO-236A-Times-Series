#Aula 10 - Modelos de Regressão com Séries de tempo

rm(list=ls(all.names=TRUE))

#Leitura de Dados INTDEF.xlsx
Dados=ts(INTDEF,start=1948,freq=1)

layout(1:3)
plot(Dados[,2],xlab='Ano',ylab='Taxa de Juros - I3')
plot(Dados[,3],xlab='Ano',ylab='Taxa de Inflação - INF')
plot(Dados[,4],xlab='Ano',ylab='Déficit - DEF')

#Modelo de regressão
modelo1=lm(I3~INF + DEF, data=Dados)
summary(modelo1)
anova(modelo1)

library(car)
linearHypothesis(modelo1,'INF=0')
##########################
#Exemplo 2
rm(list=ls(all.names=TRUE))

#Leitura dos dados FERTIL3.xlsx
Dados=ts(FERTIL3,start=1913,freq=1)

layout(1:2)
plot(Dados[,2],xlab='Ano',ylab='GRF')
plot(Dados[,3],xlab='Ano',ylab='PE')

year=FERTIL3[,1]

#Variáveis Binárias
ww2=ts(as.numeric((year >=1941) & (year<=1945)))

View(ww2)
pill=ts(as.numeric(year>=1963))

#Modelo de regressão
modelo2=lm(GFR ~ PE + ww2+ pill,data=Dados)
summary(modelo2)

#Modelo dinâmico
install.packages('dynlm')
library(dynlm)

n=length(Dados[,1])
n

modelo3 = lm(GFR[3:n] ~ PE[3:n] + PE[2:(n-1)] + 
               PE[1:(n-2)] + ww2[3:n] + pill[3:n], data=Dados )
summary(modelo3)


#Correlação
cor(FERTIL3$PE[3:n],FERTIL3$PE[2:(n-1)])

cor(FERTIL3$PE[3:n],FERTIL3$PE[1:(n-2)])

layout(1:1)

acf(FERTIL3$PE)

#Análise da significância conjunta de pe
install.packages('car')
library(car)

y=FERTIL3$GFR[3:n]
x1=FERTIL3$PE[3:n]
x2=FERTIL3$PE[2:(n-1)]
x3=FERTIL3$PE[1:(n-2)]

reg=lm(y ~ x1+x2+x3 +ww2[3:n]+pill[3:n],data=Dados)
  
linearHypothesis(reg, 
                 c('x1=0', 'x2=0',
                   'x3=0'),test='F',data=Dados)

linearHypothesis(reg, c('x2=0','x3=0'),test='F',data=Dados)

PLP = reg$coef[2] + reg$coef[3] + reg$coef[4]
PLP

linearHypothesis(reg, c('x1+x2+x3=0'),test='F',data=Dados)


#Modelo de Investimento Imobiliário

rm(list=ls(all.names=TRUE))

lninvpc=log(HSEINV$INV/HSEINV$POP)
lnprice = log(HSEINV$PRICE)

lninvpc=ts(lninvpc,start=1947, freq=1)
lnprice=ts(lnprice,start=1947,freq=1)

layout(1:2)
plot(lninvpc,xlab='ano',ylab='Investimento')
plot(lnprice, xlab='ano', ylab='preço')

reg1=lm(lninvpc ~ lnprice)
summary(reg1)

trend=ts(1:length(lninvpc),start=1947,freq=1)

reg2=lm(lninvpc ~ lnprice + trend)
summary(reg2)

































