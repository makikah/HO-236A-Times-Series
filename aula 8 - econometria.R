#Aula 8 - Variáveis Binárias

#Carregando os dados:

library(readxl)
Dados_CO2 <- read_excel("R:/HO-231-Rosangela/Dados/Dados_CO2.xls")
View(Dados_CO2)

#Modelo 1

#pib = Dados_CO2[,3], nrow=length(Dados_CO2)

#De outra forma, pode-se extrair a coluna 3 escrevendo:

pib=Dados_CO2$pib

n = length(pib)

alta=rep(0,n)
View(alta)

for(i in 1:n){if(pib[i]>=12000)alta[i]=1}

#Interpretação: tem-se um n = 169. Criou-se um "alta" = dummy. O "for" faz a programação do que se quer. Exemplo: se PIB[1] é maior ou igual a 12000, então alta [1] = 1.

dados = cbind(pib,alta)
View(dados)

#Especificada a variável dummy, constrói-se o modelo:

modelo1 = lm (co2 ~ alta + setor2, data = Dados_CO2)
summary(modelo1)

#Portanto, para países com pib >= a 12.000, o valor médio de Co2 emitido pelos países desse grupo é em média 8,2 unidades a mais que os países com pib inferior a 12.000, ceteris paribus. Enquanto que o aumento de uma unidade no setor2 implica, ceteris paribus, um aumento de 0,11 unidades no Co2.
#observe que alterar as variáveis para binárias (dividi-las em grupos de países), não afeta a espeficicação do modelo.

#Binárias com k categorias

media=rep(0,n)

for(i in 1:n){if(pib[i]>=4000 & (pib[i]<12000))media[i]=1}

dados2=cbind(pib,media)
View(dados2)

modelo2 = lm (co2 ~ media + alta + setor2, data = Dados_CO2)
summary(modelo2)

#Países com renda < 4.000, têm-se: co2^ = -0,998 + 0,093setor2. Países com renda média, têm-se: co^2 = -0,998 + 3,97media + 0,093setor2. Países com renda alta, têm-se: Co2^ = -0,998 + 9,1alta + 0,093setor2.
#Interpretação: países de renda alta emitem, em média, 9 unidades a mais que os países de renda baixa, ceteris paribus. Os países de renda média emitem, em média, 4 unidades a mais que os países de renda baixa, ceteris paribus.
#Observa-se que, partindo de um modelo linear estatisticamente significativo, então a tranformação do modelo em grupos de variáveis binárias pode afetar a significância do modelo.

#Binárias em funções logarítmicas

#Tomemos um modelo log-lin

modelo3 = lm (log(co2) ~ alta + setor2, data = Dados_CO2)
summary(modelo3)

#Interpretação coeficiente de alta:
binária=exp(modelo3$coefficients[2])-1

#Logo, emissões dos países de renda alta são, em média, quase 8 (ou quase 800% maior) vezes maior que os demais países (resto do mundo).

#Tomemos mais um modelo log-lin

modelo4 = lm (log(co2) ~ alta + media + setor2, data = Dados_CO2)
summary(modelo4)
binaria_alta=exp(modelo4$coefficients[2])-1
binaria_media=exp(modelo4$coefficients[3])-1


#interpretação: os países de renda alta emitem, em média, 12,1 vezes mais que os países de renda baixa (i.e., a base de comparação), ceteris paribus. Os países de renda média emitem, em média, 4,7 vez a mais que os países de renda baixa, ceteris paribus.

#Coeficiente angular interativo

interacao = Dados_CO2$setor2*alta
modelo5 = lm (log(co2) ~ alta + setor2 + interacao, data = Dados_CO2)
summary(modelo5)

#Ver interpretação no slide.

#Mudança estrutural

install.packages('car')
library(car)
linearHypothesis(modelo5, c ('alta=0','interacao=0'), test='F', data = Dados_CO2)

#Rejeita-se a hipótese nula. Logo, não se pode rejeitar a hipótese de mudança estrutural; embora não se saiba onde esta mudança ocorreu (se foi via interacao e/ou alta). Observando individualmente, verifica-se que "alta" é estatisticamente significante, enquanto "interacao" não é; logo, a mudança estrutural se dá via variável "alta".