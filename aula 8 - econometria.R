#Aula 8 - Vari�veis Bin�rias

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

#Interpreta��o: tem-se um n = 169. Criou-se um "alta" = dummy. O "for" faz a programa��o do que se quer. Exemplo: se PIB[1] � maior ou igual a 12000, ent�o alta [1] = 1.

dados = cbind(pib,alta)
View(dados)

#Especificada a vari�vel dummy, constr�i-se o modelo:

modelo1 = lm (co2 ~ alta + setor2, data = Dados_CO2)
summary(modelo1)

#Portanto, para pa�ses com pib >= a 12.000, o valor m�dio de Co2 emitido pelos pa�ses desse grupo � em m�dia 8,2 unidades a mais que os pa�ses com pib inferior a 12.000, ceteris paribus. Enquanto que o aumento de uma unidade no setor2 implica, ceteris paribus, um aumento de 0,11 unidades no Co2.
#observe que alterar as vari�veis para bin�rias (dividi-las em grupos de pa�ses), n�o afeta a espeficica��o do modelo.

#Bin�rias com k categorias

media=rep(0,n)

for(i in 1:n){if(pib[i]>=4000 & (pib[i]<12000))media[i]=1}

dados2=cbind(pib,media)
View(dados2)

modelo2 = lm (co2 ~ media + alta + setor2, data = Dados_CO2)
summary(modelo2)

#Pa�ses com renda < 4.000, t�m-se: co2^ = -0,998 + 0,093setor2. Pa�ses com renda m�dia, t�m-se: co^2 = -0,998 + 3,97media + 0,093setor2. Pa�ses com renda alta, t�m-se: Co2^ = -0,998 + 9,1alta + 0,093setor2.
#Interpreta��o: pa�ses de renda alta emitem, em m�dia, 9 unidades a mais que os pa�ses de renda baixa, ceteris paribus. Os pa�ses de renda m�dia emitem, em m�dia, 4 unidades a mais que os pa�ses de renda baixa, ceteris paribus.
#Observa-se que, partindo de um modelo linear estatisticamente significativo, ent�o a tranforma��o do modelo em grupos de vari�veis bin�rias pode afetar a signific�ncia do modelo.

#Bin�rias em fun��es logar�tmicas

#Tomemos um modelo log-lin

modelo3 = lm (log(co2) ~ alta + setor2, data = Dados_CO2)
summary(modelo3)

#Interpreta��o coeficiente de alta:
bin�ria=exp(modelo3$coefficients[2])-1

#Logo, emiss�es dos pa�ses de renda alta s�o, em m�dia, quase 8 (ou quase 800% maior) vezes maior que os demais pa�ses (resto do mundo).

#Tomemos mais um modelo log-lin

modelo4 = lm (log(co2) ~ alta + media + setor2, data = Dados_CO2)
summary(modelo4)
binaria_alta=exp(modelo4$coefficients[2])-1
binaria_media=exp(modelo4$coefficients[3])-1


#interpreta��o: os pa�ses de renda alta emitem, em m�dia, 12,1 vezes mais que os pa�ses de renda baixa (i.e., a base de compara��o), ceteris paribus. Os pa�ses de renda m�dia emitem, em m�dia, 4,7 vez a mais que os pa�ses de renda baixa, ceteris paribus.

#Coeficiente angular interativo

interacao = Dados_CO2$setor2*alta
modelo5 = lm (log(co2) ~ alta + setor2 + interacao, data = Dados_CO2)
summary(modelo5)

#Ver interpreta��o no slide.

#Mudan�a estrutural

install.packages('car')
library(car)
linearHypothesis(modelo5, c ('alta=0','interacao=0'), test='F', data = Dados_CO2)

#Rejeita-se a hip�tese nula. Logo, n�o se pode rejeitar a hip�tese de mudan�a estrutural; embora n�o se saiba onde esta mudan�a ocorreu (se foi via interacao e/ou alta). Observando individualmente, verifica-se que "alta" � estatisticamente significante, enquanto "interacao" n�o �; logo, a mudan�a estrutural se d� via vari�vel "alta".