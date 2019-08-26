#Aula 5 - Teste de Raiz Unitária 3 - Quebra Estrutural

N = 100

set.seed(123)
aleat = ts(rnorm(N,0,1))

y1 = ts (rep(0,N))  #um vetor que repete a série 100 vezes.
y2 = 2

D1 = ts (rep(0,N))
D2 = ts (rep(0,N))

for (t in 1:N) {
  if (t > 40)
    D1[t] = 3
  if (t = 41) {
    D2[t] = 3}
}

for (t in 2:N) {
  y1[t] = 0.5*y1[t-1]+aleat[t]+D1[t]
  y2[t] = y2[t-1]+aleat[t]+D2[t]
}

par(mfrow = c(1,2))
plot(y1)
plot(y2)

#Principais testes de quebras estruturais
#1) Teste de Chow (1960) - realiza o teste para um período em que se sup?e/conhece 
## a exist?ncia de quebra estrutural (i.e., quebra ex?gena).
#2) Teste de Chow/Zeileis et al (2001) - realiza o teste para vários períodos. 
## Faz-se a análise em janelas de percentis da série. Ressalta-se que se pode 
## restringir os limites da an?lise de quebra estrutural da série. Esse encontra 
## apenas uma quebra estrutal, se houver. A hipótese nula afirma que não há quebra 
## estrutural.

# Carregam-se os dados

library(readxl)
View(CambioComercial)


library(strucchange)

cambio = ts (CambioComercial[,2], start = c(1995,1), frequency = 12)
par(mfrow = c(1,1))
plot(cambio)
 
#Teste de Chow/Zeileis et al (2001)

cambio.chow = Fstats(cambio ~ 1, from = 0.15)
breakpoints(cambio.chow)

#Aplicando o teste para um período restrito:

cambio1 = ts (CambioComercial[,2], start = c(1995,1), end = c(2008, 6), frequency = 12)
par(mfrow = c(1,1))
plot(cambio1)

cambio1.chow = Fstats(cambio1 ~ 1, from = 0.15)
breakpoints(cambio1.chow) #mostra o ponto da quebra quebra estrutural
sctest(cambio1.chow) #Isto ?, rejeita-se a hip?tese nula de inexistência de quebra 
                     ## estrutural para qualquer nível de significência.
lines(breakpoints(cambio1.chow))  # desenha uma linha vertical pontilhada no gráfico 
                                  # no exato momento t da quebra estrutural

#3) Teste de Bai e Perron (2003) - identifica mais de uma quebra estrutural, se houver

cambio1.bp = breakpoints(cambio1 ~ 1, h = 0.15) #nota-se que a função "breakpoints" 
                                                #aponta, no máximo, cinco quebras 
                                                #estruturais, se houver
summary(cambio1.bp)
plot(cambio1.bp)

# Teste de Raiz Unitária com Quebra Estrutural (PERRON, 1989). Há três tipos de 
# quebras estruturais possíveis para um randon walk com drift: (i) uma mudança 
# de nível da série; (ii) uma mudança de inclinação; (iii) ambas as mudanças

# Teste de Perron
# Teste de quebra estrutural ex?gena
N = length(cambio1)

DP = ts (rep(0,N))
DL = ts (rep(0,N))
DS = ts (rep(0,N))

Tb = 49 #indica-se o momento ocorreu a quebra

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

# Ajuste do modelo da hipótese alternativa - Escolha do modelo A

# Passo a passo

#a) estimação do modelo

trend = ts (seq(1,N))
modelo = lm (cambio1 ~ trend + DL)
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
lambda   #verificando na tabela de Perron (1989), nota-se que o valor da estatística não 
        # pertence ? região crítica; assim, não se rejeita a hipótese nula de uma raiz unitária.

#4) Teste de Zivolt e Andrews - quebra estrutural endógena/desconhecida. Indica apenas um único 
# ponto de quebra

cambio1.za = ur.za(cambio1, model = 'intercept', lag = NULL)
summary(cambio1.za) # não se rejeita a hipótese nula de random walk (uma raiz unitária)
plot(cambio1.za)

cambio1.za2 = ur.za (cambio1, model = 'trend', lag = NULL)
summary(cambio1.za2) # não se rejeita a hipótese nula de random walk (uma raiz unitária)
plot(cambio1.za2)  # uma vez que se escolheu um modelo com tendência, nota-se que o ponto 
                   # de quebra indicado está associado a mudança na tendência da série.

cambio1.za3 = ur.za (cambio1, model = 'both')
summary(cambio1.za3) #rejeita-se a hipótese nula de raiz unitária a 5%, mas não a 1%.
plot(cambio1.za3)
