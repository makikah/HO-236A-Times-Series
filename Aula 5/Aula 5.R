## Aula Teste de RU

ipca = ts(IPCA[,2], start = c(1995, 1), frequency = 12)
par(mfrow = c(1, 2))
plot(ipca, ylab = "IPCA", xlab = "tempo")
acf(ipca)

## Test ADF
library(urca)
library(stargazer)
## Ho: p = 0 càd la série n'est pas stationnaire
## H1: p < 0 càd la série est stationnaire

ipca.df = ur.df(ipca, type = "trend", lags = 0)
plot(ipca.df) ## Nào estacionário colocando lags = 0

ipca.df1 = ur.df(ipca, type = "trend", lags = 12)
plot(ipca.df1)
summary(ipca.df1)

## Critère d'information AIC et BIC
ipca.aic = ur.df(ipca, type = "trend", lags = 16, selectlags = "AIC")
plot(ipca.aic)
summary(ipca.aic) ## Les résultats nous montre qu'il faut rejeter l'Hypothèse nulle. Ho: 
                  ## Une racine unitaire càd la série est stationnaire pour ce modèle. 

ipca.bic = ur.df(ipca, type = "trend", lags = 16, selectlags = "BIC")
plot(ipca.bic)
summary(ipca.bic) ## série est stationnaire

ipca.af1 = ur.df(ipca, type = "drift", lags = 0)
plot(ipca.af1)
summary(ipca.af1) ## La serie est stationnaire, car, Value of tes-stat est superieur au critical values for
                  ## test stats (tau2, càd au seuil de 5%)

ipca.bf2 = ur.df(ipca, type = "drift", lags = 12)
plot(ipca.af1)
summary(ipca.af1)

ipca.aic1 = ur.df(ipca, type = "drift", lags = 16, selectlags = "AIC")
plot(ipca.aic1)
summary(ipca.aic1)

ipca.bic1 = ur.df(ipca, type = "trend", lags = 16, selectlags = "BIC")
plot(ipca.bic1)


## POUVOIR DU TEST DE DICKEY-FULLER
### 1. Erreur du type 1 : probabilidade de rejeitar a hipótese nula quando esta hipótese é verdadeira.
### 2. Erreur du type 2 : probabilidade de não rejeitar a hipótese nulaquando a hipótese alternativa 
###    é verdadeira. Le test ADF a peu de pouvoir explicatif pour se faire on fait le test de Phillips-Perron

## Test de Phillips-Perron
### O teste baseia-se na mesma hipótese nula e estrutura do teste DF

ipca.pp = ur.pp(ipca, type = "Z-tau", model = "trend", lags = "short")
plot(ipca.pp)
stargazer(ipca.pp, type = "text", keep.stat = c("n", "rsq", "adj.rsq", "f"),
          out = "IPCA.txt")
summary(ipca.pp)

## Test de DF-GLS ou ERS (ERS–Elliot, Rothenberg e Stock)
ipca.ers = ur.ers(ipca, type = "DF-GLS", model = "constant", lag.max = 1)
plot(ipca.ers)
summary(ipca.ers) ## Au seuil de 10%, on peut rejeter l'Ho. 

## Résumé: ADF/ERS/PP, l'hypothèse nulle est Ho: 1 RU (présence d'une racine unitaire)

## Test KPSS

## Ici l'Ho: serie est stationnaire, H1: série est non stationnaire.

### Kwiatkowski, Phillips, Schmidt & Shin (1992) desenvolveram um teste de raiz unitária denominado KPSS.
### Teste de Dickey e Fuller: baixo poder = o teste tende a não rejeitar a hipótese nula para uma 
### infinidade de séries econômicas.
### Na prática, o teste KPSS é aplicado para checar a conclusão dos testes DF ou ADF.

ipca.kpss = ur.kpss(ipca, type = "mu", lags = "short")
summary(ipca.kpss) ## A 10% on rejet Ho. 
plot(ipca.kpss)
