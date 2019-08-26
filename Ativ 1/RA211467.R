#Atividade II - quebra estrutral com raiz unitária

#Aluno: João Paulo Farias Fenelon   RA: 211467

#1)Pede-se a verificação de quebra estrutural da seguinte série:

library(readxl)
Outros_Investimentos <- read_excel("G:/Meu Drive/UNICAMP/3º Semestre/HO 236 - Econometria de Séries Temporais/Aula 4/Outros Investimentos.xlsx")
View(Outros_Investimentos)

library(strucchange)

#Seja a série:
inv = ts (Outros_Investimentos[, 2], start=c(2001, 4), frequency = 4)
par(mfrow=c(1, 1))
plot(inv, main = "Outros Investimentos" , ylab = "US$ Milhões", xlab = 'Período')

#Verificação de existência de ponto de quebra estrutural usando o teste Chow/Zeileis et al (2001):

inv.chow = Fstats(inv ~ 1, from = 0.15)
breakpoints(inv.chow) #mostra o ponto da quebra estrutural (= observação 22 ou jan/2007)
sctest(inv.chow)

#Isto é, rejeita-se a hipótese nula em que se afirma não existir quebra estrutural. Graficamente, o ponto de quebra pode ser indicado pela seguinte linha tracejada:

lines(breakpoints(inv.chow))

#Verificação de existência de ponto de quebra estrutural utilizando o teste de Bai e Perron (2002).

inv.bp = breakpoints(inv ~ 1, h = 0.15, breaks = NULL)  #Com h = 0,15, idenfica-se a possibilidade mais de cinco quebras estruturais.
summary(inv.bp)
plot(inv.bp)

#2) Teste de RU com quebra estrutural conhecida (exógena) proposto por Perron (1989).

N = length(inv)

DP = ts (rep(0,N))
DL = ts (rep(0,N))
DS = ts (rep(0,N))

Tb = 22

for (t in 1:N) {
  if (t == Tb + 1)
    DP[t] = 1
  if (t>Tb){
    DL[t] = 1
    DS[t] = t - Tb
  }
}

View(DP)
View(DL)
View(DS)

#Ajuste do modelo da hipótese alternativa - Escolha do modelo A. Isto é, testa-se um modelo de tendência estacionária mas com mudança permanente de nível.

#Passo a passo

#a) estimação do modelo

trend = ts (seq(1,N))
modelo = lm (inv ~ trend + DL)
summary(modelo)

#b) Obtenção dos resíduos

res = modelo$residuals
plot.ts (res)

#c) Teste ADF aplicado aos resíduos

library('urca')
res.df = ur.df(res, type = 'none', lags = 6, selectlags = 'BIC')
summary(res.df)
plot(res.df)

res1.df = ur.df(res, type = 'none', lags = 0) #teste DF para fins comparativos
summary(res1.df)
plot(res1.df)

lambda = Tb/N
lambda

#Assim, nota-se que o valor da estatística tau-A -2,8459 é menor em valor absoluto do que seu valor crítico -3,76 associado ao valor de lambda = 0,3, conforme a Tabela 4.1 de Perron (1989); assim, não se rejeita a hipótese nula de existência de uma raiz unitária a 5% com presença de quebra estrutural em um modelo com mudança no nível da série.

#3) Teste de RU com quebra estrutural desconhecida (endógena) proposto por Zivot e Andrews

inv.za=ur.za(inv, model='intercept')
summary(inv.za)
plot(inv.za)

#O modelo com mudança no nível da série indica que o ponto de quebra estrutural potencial ocorre na posição 59 da série (terceiro semestre de 2014). O valor da estatística do teste é menor em valor absoluto em relação aos seus valores críticos. Portanto, não se rejeita a hipótese nula de presença de uma raiz unitária.

inv.za2=ur.za(inv, model='trend')
summary(inv.za2)
plot(inv.za2)

#Considerando o modelo com mudança na inclinação, não se rejeita a hipótese nula de raiz unitária a 1%, mas rejeita-se a 5%.

inv.za3=ur.za(inv,model='both')
summary(inv.za3)
plot(inv.za3)

#Por último, considerando um modelo com mudança no nível e na tendência, não se rejeita a hipótese nula de raiz unitária a 1%, mas rejeita-se a 5%.