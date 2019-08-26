#Variáveis Instrumentais

dados=subset(MROZ,!is.na(MROZ$WAGE))

n=nrow(dados)
lwage=log(dados$WAGE)
educ=dados$EDUC
exper=dados$EXPER
exper2=(dados$EXPER)^2
motheduc=dados$MOTHEDUC
fatheduc=dados$FATHEDUC
huseduc=dados$HUSEDUC

reg1=lm(lwage ~educ + exper + exper2)
summary(reg1)

#Usar motheduc como instrumento
#verificar a correlação
reg.aux=lm(educ~motheduc)
summary(reg.aux)

install.packages('AER')
library(AER)

reg.iv=ivreg(lwage~educ + exper+exper2 | 
               motheduc + exper + exper2)
summary(reg.iv)

#Inclusão dos instrumentos 
#fatheduc huseduc e motheduc
#Verificar instrumentos fortes
reg2.aux=lm(educ~motheduc + fatheduc + 
              huseduc +exper + exper2)
summary(reg2.aux)

linearHypothesis(reg2.aux,
                 c('motheduc=0','fatheduc=0',
                   'huseduc=0'))

reg2.iv=ivreg(lwage~educ+exper+ exper2|
                motheduc+fatheduc+
                huseduc+exper+exper2)
summary(reg2.iv)

#Teste de heterocedasticidade
bptest(reg2.iv)

#Estatistica Robusta
summary(reg2.iv,vcov=sandwich)

#Testes de Instrumentos
summary(reg2.iv,vcov=sandwich,
        diagnostics = TRUE)






















































