## Atividade 1_Henri Makika

# 1) Elaborar um gráfico com a série (ipeadata)

dados = ipeadata_01_04_2019_10_32_
Pib = ts(dados[,2], start = c(1957, 1), frequency = 4)
par(mfrow = c(1,2))
plot(Pib, main = "PIB_USA", xlab = "Trim/Ano", ylab = "PIB")
grid()
acf(Pib, main = "Correlation")


# 2) Faça o correlograma (função de autocorrelação)
par(mfrow = c(2,2))

plot(Pib, main = "PIB de USA", ylab = "PIB")
grid()
acf(Pib, main = "Autocorrelation", ylab = "FAC", xlab = "Lags")

DPIB = diff(Pib)
plot(DPIB, main = "PIB en différence", ylab = "DPIB")
grid()
acf(DPIB, main = "Autocorrelation", ylab = "FAC", xlab = "Lags")

# 3) A partir do gráfico da série e do correlograma qual a sua conclusão sobre a estacionariedade da série?

### Nos graphiques ne nous montrent pas la stationnarité de la série. On peut donc conlure que notre 
### série n'est pas stationnaire. Et maintenant nous nous vérifions à partir des tests de racine unitaire.

# 4) Faça o teste de raiz unitária ADF, explicitando e analisando cada etapa do teste. 
   # A partir deste resultado qual a conclusão sobre a estacionariedade da série?
library(TSA)
library(urca)

adf0 = ur.df(Pib, type = "trend", lags = 0)
summary(adf0)
plot(adf0)
## la série n'est pas stationnaire. L'hypothèse nulle est n'est pas acceptée.  

adf1 = ur.df(Pib, type = "trend", lags = 12)
summary(adf1)
plot(adf1)
## En considérant lags = 1, nous pouvons déjà conclure que la série est stationnaire au seuil de 1%. 
## Mais vérifions en utilisant le critère akaike et bic.

## Critère AIC et BIC Pour tester la stationnarité de série
aic.adf = ur.df(Pib, type = "trend", lags = 12, selectlags = "AIC")
plot(aic.adf)
summary(aic.adf)

bic.adf = ur.df(Pib, type = "trend", lags = 12, selectlags = "BIC") 
plot(bic.adf)
summary(bic.adf)

bic.adf2 = ur.df(Pib, type = "drift", lags = 8, selectlags = "BIC")
plot(bic.adf2)
summary(bic.adf2) ## La série a une racine unitaire. Ho est peut être accepté. 

## Testons notre série PIB_USA en différence
DPIB.adf = ur.df(DPIB, type = "drift", lags = 8, selectlags = "BIC")
plot(DPIB.adf)
summary(DPIB.adf)

## Nous concluons que notre série de PIB_USA poursuit une racine unitaire, cela étant, 
## la variation est stationnaire et le modèle à estimer est AR(1). 

DPIB.adf1 = ur.df(DPIB, type = "trend", lags = 8, selectlags = "BIC")
summary(DPIB.adf1)
plot(DPIB.adf1)

DPIB.adf2 = ur.df(DPIB, type = "none", lags = 4, selectlags = "BIC")
summary(DPIB.adf2)
plot(DPIB.adf2)

# 5) Faça o Teste de PP e análise os resultados. 
#    A partir deste resultado qual a conclusão sobre a estacionariedade da série?

pib.pp = ur.pp(Pib, type = "Z-tau", model = "trend", lags = "short")
plot(pib.pp)
summary(pib.pp)

# 6) Faça o Teste de DF-GLS (ou ERS) e análise os resultados. A partir deste resultado 
#    qual a conclusão sobre a estacionariedade da série?

pib.ers = ur.ers(Pib, type = c("DF-GLS"), model = c("trend"), lag.max = 1)
plot(pib.ers)
summary(pib.ers)

# 7) Faça o Teste de KPSS e análise os resultados. A partir deste resultado qual a conclusão sobre a 
#    estacionariedade da série?

pib.kpss = ur.kpss(Pib, type = "mu", lags = "short", use.lag = NULL)
plot(pib.kpss)
summary(pib.kpss) ## Nous concluons que la série est stationnaire, donc on rejete Ho.  

##################################################################################

# Para a série encaminhada aplique os testes para analisar se há quebra estrutural.
# A partir deste resultado aplique o teste de RU com quebra estrutural conhecida 
# proposto por Perron e o teste de RU com quebra estrutural proposto por Zivot e Andrews.

## Teste de quebra estrutural
library(strucchange)

Pib = ts(dados[,2], start = c(1957, 1), frequency = 4)
par(mfrow = c(1,1))
plot(Pib, main = "PIB USA", xlab = "Ano", ylab = "PIB")
grid()

# Teste de Chow-Zeileis et al (2001)
pib.chow = Fstats(Pib ~ 1, from = 0.15)
breakpoints(pib.chow) 
sctest(pib.chow)
lines(breakpoints(pib.chow)) ## Tem quebra estructural no terceiro trimestre de 1993.
                             ## Rejeita a hipótese nula de que não existe a quebra estrutural.

# Aplicando o teste de Bai e Perron (2002)para verificar se há mais de quebras estruturais:

pib.bp = breakpoints(Pib ~ 1, h = 0.15, breaks = NULL)
summary(pib.bp) ## A série tem cinco quebras estruturais. 
plot(pib.bp)

# Teste de RU com quebra estrutural conhecido proposto por Perron (1989) :

N = length(Pib)

DP = ts(rep(0, N))
DL = ts(rep(0, N))
DS = ts(rep(0, N))

DP = ts (rep(0,N))
DL = ts (rep(0,N))
DS = ts (rep(0,N))
Tb = 147

for (t in 1:N) {
  if(t == Tb + 1)
    DP[t] = 1
  if(t > Tb){
    DL[t] = 1
    DS[t] = t - Tb
  }
}

# Ajuste do modelo da hipótese alternativa
## Estimação do modelo

trend = ts(seq(1, N))
model = lm(Pib ~ trend + DL)
summary(model)

## Obtenção dos resídos
res = model$residuals
plot(res)

## Test do ADF
library(urca)

res.df = ur.df(res, type = "none", lags = 4, selectlags = "BIC")
summary(res.df)
plot(res.df)

res0.df = ur.df(res, type = "none", lags = 0)
summary(res0.df)

lambda = Tb/N
print(lambda) ### O valor estatística en valor absoluta é 1.9512. 
## Não rejeita a hipótese nula de existência de uma raiz unitária a 1% com presença 
## de quebra estrutural em modelo com mudança no nível da série.

# Teste de RU com quebra estrutural proposto por Zivot e Andrews
# Modelo com mudança no nível
pib.za = ur.za(Pib, model = "intercept")
summary(pib.za)
plot(pib.za)
## O ponto de quebra estrutural potencial acorrei em posição 157, logo em primeiro trimestre de 1996
## O teste etatística mostra que a hipótese nula não pode ser rejeitada em 5%. Há presença de RU. 

# Modelo com mudança na inclinação (tendência)
pib0.za = ur.za(Pib, model = "trend")
summary(pib0.za)
plot(pib0.za)
## Não rejeita a H0.

# Modelo com mudança no nível e na tendência
pib1.za = ur.za(Pib, model = "both")
summary(pib1.za)
plot(pib1.za)
## Não rejeita a hipótese nula da presença de raiz unitária. 