### ATIVIDADE 3 - Henri Makika - RA: 211042

# Para a série encaminhada aplique os testes para analisar se há quebra estrutural.
# A partir deste resultado aplique o teste de RU com quebra estrutural conhecida 
# proposto por Perron e o teste de RU com quebra estrutural proposto por Zivot e Andrews.

## Teste de quebra estrutural
library(strucchange)
dados = ipeadata_01_04_2019_10_32_

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
plot(res0.df)

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