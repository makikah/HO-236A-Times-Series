#Atividade II - quebra estrutral com raiz unit�ria

#Aluno: Jo�o Paulo Farias Fenelon   RA: 211467

#1)Pede-se a verifica��o de quebra estrutural da seguinte s�rie:

library(readxl)
Outros_Investimentos <- read_excel("G:/Meu Drive/UNICAMP/3� Semestre/HO 236 - Econometria de S�ries Temporais/Aula 4/Outros Investimentos.xlsx")
View(Outros_Investimentos)

library(strucchange)

#Seja a s�rie:
inv = ts (Outros_Investimentos[, 2], start=c(2001, 4), frequency = 4)
par(mfrow=c(1, 1))
plot(inv, main = "Outros Investimentos" , ylab = "US$ Milh�es", xlab = 'Per�odo')

#Verifica��o de exist�ncia de ponto de quebra estrutural usando o teste Chow/Zeileis et al (2001):

inv.chow = Fstats(inv ~ 1, from = 0.15)
breakpoints(inv.chow) #mostra o ponto da quebra estrutural (= observa��o 22 ou jan/2007)
sctest(inv.chow)

#Isto �, rejeita-se a hip�tese nula em que se afirma n�o existir quebra estrutural. Graficamente, o ponto de quebra pode ser indicado pela seguinte linha tracejada:

lines(breakpoints(inv.chow))

#Verifica��o de exist�ncia de ponto de quebra estrutural utilizando o teste de Bai e Perron (2002).

inv.bp = breakpoints(inv ~ 1, h = 0.15, breaks = NULL)  #Com h = 0,15, idenfica-se a possibilidade mais de cinco quebras estruturais.
summary(inv.bp)
plot(inv.bp)

#2) Teste de RU com quebra estrutural conhecida (ex�gena) proposto por Perron (1989).

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

#Ajuste do modelo da hip�tese alternativa - Escolha do modelo A. Isto �, testa-se um modelo de tend�ncia estacion�ria mas com mudan�a permanente de n�vel.

#Passo a passo

#a) estima��o do modelo

trend = ts (seq(1,N))
modelo = lm (inv ~ trend + DL)
summary(modelo)

#b) Obten��o dos res�duos

res = modelo$residuals
plot.ts (res)

#c) Teste ADF aplicado aos res�duos

library('urca')
res.df = ur.df(res, type = 'none', lags = 6, selectlags = 'BIC')
summary(res.df)
plot(res.df)

res1.df = ur.df(res, type = 'none', lags = 0) #teste DF para fins comparativos
summary(res1.df)
plot(res1.df)

lambda = Tb/N
lambda

#Assim, nota-se que o valor da estat�stica tau-A -2,8459 � menor em valor absoluto do que seu valor cr�tico -3,76 associado ao valor de lambda = 0,3, conforme a Tabela 4.1 de Perron (1989); assim, n�o se rejeita a hip�tese nula de exist�ncia de uma raiz unit�ria a 5% com presen�a de quebra estrutural em um modelo com mudan�a no n�vel da s�rie.

#3) Teste de RU com quebra estrutural desconhecida (end�gena) proposto por Zivot e Andrews

inv.za=ur.za(inv, model='intercept')
summary(inv.za)
plot(inv.za)

#O modelo com mudan�a no n�vel da s�rie indica que o ponto de quebra estrutural potencial ocorre na posi��o 59 da s�rie (terceiro semestre de 2014). O valor da estat�stica do teste � menor em valor absoluto em rela��o aos seus valores cr�ticos. Portanto, n�o se rejeita a hip�tese nula de presen�a de uma raiz unit�ria.

inv.za2=ur.za(inv, model='trend')
summary(inv.za2)
plot(inv.za2)

#Considerando o modelo com mudan�a na inclina��o, n�o se rejeita a hip�tese nula de raiz unit�ria a 1%, mas rejeita-se a 5%.

inv.za3=ur.za(inv,model='both')
summary(inv.za3)
plot(inv.za3)

#Por �ltimo, considerando um modelo com mudan�a no n�vel e na tend�ncia, n�o se rejeita a hip�tese nula de raiz unit�ria a 1%, mas rejeita-se a 5%.