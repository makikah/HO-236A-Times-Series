set.seed(1)
E = rnorm(240)
print(E)
X = cumsum(E)
print(X)

plot(X, type = "l", xlab = "Time")

# Test de Dickey-Fuller
library(urca)

## On peut regarder s’il y a une constante non nulle (on parlera de modèle avec drift) 
## voire une tendance linéaire (on parlera de trend).

df = ur.df(X, type = "none", lags = 1)
summary(df)

# La région critique du test est ici (pour un seuil à 5%) l’ensemble des valeurs 
# inférieures à -1.95. Or ici la statistique de test est ici -0.73, on est alors dans 
# la région d’acceptation du test, et on va retenir l’hypothèse de racine unité, 
# i.e. la série n’est pas stationnaire. Mais peut-être faudrait-il juste prendre en 
# compte une constante ?

df1 = ur.df(X, type = "drift", lags = 1)
summary(df1)

# Deux statistiques de test sont calculées, ici: la première relative à la racine unité, 
# la seconde relative à la constante.  On observe ici que la statistique de test pour 
# le test de racine unité (-2.3) est ici supérieure à toutes les valeurs critiques 
# associées (données sur la première ligne). On va encore accepter l’hypothèse de 
# racine unité. Mais le modèle était peut-être faux, et peut-être avait-on en fait en 
# tendance linéaire ?

df2 = ur.df(X, type = "trend", lags = 1)
summary(df2)

# On obtient cette fois trois statistique, la première relative au test de racine unité, 
# et les deux suivantes aux tests sur la constante, et sur la tendance (la pente de 
# l’ajustement linéaire). Là encore, la valeur de test (-1.98) excède les valeurs 
# critiques: la p-value serait ici supérieure à 10%. On va là encore accepter 
# l’hypothèse de racine unité.
# Mais la modélisation autorégressive à l’ordre 1 était peut-être elle aussi fausse. 
# Aussi, il existe un test de Dickey-Fuller augmenté. L’idée est de considérer, 
# de manière beaucoup plus générale, un processus autorégressif à un ordre supérieur.

df3 = ur.df(X, type = "none", lags = 2)
summary(df3)

# la valeur de la statistique de test excède là encore les valeurs critiques, i.e. 
# la p-value dépasse 10%. Si on rajoute une constante.

df4 = ur.df(X, type = "drift", lags = 2)
summary(df4)

# on valide encore l’hypothèse de racine unité et de même avec une tendance linéaire.

df5 = ur.df(X, type = "trend", lags = 2)
summary(df5)

# On notera toutefois que ces trois modèles nous suggèrent de ne pas retenir autant de 
# retard, qui ne semblent pas significatifs.
# On peut bien entendu faire ces tests sur de vraies données par exemple le niveau du 
# Nil.

library(datasets)
NILE = Nile

base = read.table("http://freakonometrics.free.fr/basedata.txt", header=TRUE)
Y = base[,"R"]
Y = Y[(base$yr >= 1960) & (base$yr <= 1996.25)]
TAUX = ts(Y, frequency = 4, start = c(1960, 1))
plot(TAUX)

# Par exemple, sur cette dernière, on rejette l’hypothèse de stationnarité

df6 = ur.df(TAUX, type = "drift", lags = 3)
summary(df6)

# Mais toutes sortes d’autres tests (plus robustes) peuvent être faits. Les plus 
# connus sont le test de Philips-Perron et le test dit KPSS. Pour ce dernier, il faut 
# spécifier s’il l’on suppose que la série est de moyenne constante, ou si une 
# tendance doit être prise en compte. Si l’on suppose une constante non-nulle, 
# mais pas de tendance, on utilise:

kpss = ur.kpss(TAUX, type = "mu")
summary(kpss)

# alors que pour prendre en compte une tendance :

kpss1 = ur.kpss(TAUX, type = "tau")
summary(kpss1)
plot(kpss1)

# Derrière se cache un teste du multiplicateur de Lagrange. L’hypothèse nulle 
# correspond à l’absence de racine unité: plus la statistique de test est grande, 
# plus on s’éloigne de la stationnarité (hypothèse nulle). Avec ces deux tests, 
# on rejette là encore l’hypothèse de stationnarité de notre marche aléatoire 
# simulée.
# Pour le test de Philips-Perron, on a un test de type Dickey-Fuller :

PP.test(TAUX)
PP.test(diff(TAUX))

pp = ur.pp(TAUX, type = "Z-tau", model = "trend", lags = "short")
summary(pp)
