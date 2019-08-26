#Instrumentais - Curva de Phillips

dados=ts(Phillips,start=1948,freq=1)
n=nrow(dados)

y=diff(Phillips$INF)
reg1=lm(y~UNEM[2:n],data=dados)
summary(reg1)

#Instrumento UNEM(t-1)
reg.aux=lm(UNEM[2:n] ~UNEM[1:(n-1)],data=dados)
summary(reg.aux)

rev.iv=ivreg(y ~UNEM[2:n]|
               UNEM[1:(n-1)],data=dados)
summary(rev.iv)

bptest(rev.iv)
bgtest(rev.iv)

summary(rev.iv,diagnostics = TRUE)














