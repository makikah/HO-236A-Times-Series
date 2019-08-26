#Aula 5

dados=Dados_CO2

#Gráfico de dispersão
pairs(dados[,2:4], panel=panel.smooth)

#Ajuste Modelo Linear
linear=lm(co2 ~ pib + setor2, data=dados)
summary(linear)

#Ajuste Modelo log-lin
loglinear=lm(log(co2) ~ pib + setor2, data=dados)
summary(loglinear)

#Ajuste Modelo log-log
loglog=lm(log(co2) ~ log(pib) + setor2, data=dados)
summary(loglog)

#Intervalo de Confiança
confint(loglog,conf.level=0.95)


install.packages('stargazer')
library(stargazer)

getwd()
setwd('R:\\HO-231-Rosangela\\Dados')
stargazer(linear,loglinear,loglog,type='text',digits=8,
          column.labels=c('Linear','Log-Lin','Log-Log'),
          keep.stat=c('n','rsq','adj.rsq','f'),out='saida.txt')

#Normalidade
res=loglog$residuals

hist(res)
par(new=TRUE)
plot(density(res),main='',xlab='',ylab='')

install.packages('normtest')
library(normtest)

jb.norm.test(res)
shapiro.test(res)

#Gráfico de Dispersão 
#Resíduos e Valores estimados de CO2
est_co2=loglog$fitted.values

plot(est_co2,res)
text(est_co2,res,as.character(dados$sigla),cex=0.6,pos=1)

#Valores estimados de Y
alpha0=sum(exp(res))/length(res)
alpha0

hat_y=alpha0*exp(est_co2)

m=exp(est_co2)
alpha1=((sum(m*dados$co2))/sum(m^2))
hat_y1=alpha1*exp(est_co2)

hat_y2=exp(est_co2)

prev=cbind(hat_y,hat_y1,hat_y2)
View(prev)

#Teste de restrição
install.packages('car')
library(car)

pib2=dados$pib^2

unrestr=lm(log(co2)~setor2 + pib + pib2,data=dados)

linearHypothesis(unrestr,c('pib=0','pib2=0'),test='F')

linearHypothesis(unrestr,c('pib=0','pib2=0'),test='Chisq')

summary(unrestr)

#Análise do R2 ajustado
loglin1=lm(log(co2)~log(pib),data=dados)
summary(loglin1)

loglin2=lm(log(co2)~pib+pib2,data=dados)
summary(loglin2)

#Critério AIC
AIC(loglin1,loglin2)
































































