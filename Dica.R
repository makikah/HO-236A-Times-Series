#o comando as.numeric(conid��o) cria a bin�ria automaticamente.

#No exemplo do exemplo co2=b0+b1*setor2 + b3*alta, uma op��o mais simples para construir a bin�ria alta �:
alta=as.numeric(pib>=12000)

#O R automaticamente, a partir da fun�ao as.numeric() cria a bin�ria.
#Na aula usamos a condi��o if

#Para o caso do arquivo Dados_PIB.xlsx, temos a vai�vel sigla dos pa�ses.
#d1=bin�ria se Alemanha, d2 se Canad�, e assim sucessivamente

sigla=Dados_CO2$sigla

d1=as.numeric(sigla=='DEU') #colocamos duas vezes o sinal == pois vamos comparar caracteres
d2=as.numeric(sigla=='CAN')
d3=as.numeric(sigla=='USA')
d4=as.numeric(sigla=='ITA')
d5=as.numeric(sigla=='FRA')
d6=as.numeric(sigla=='JPN')
d7=as.numeric(sigla=='GBR')

binaria_G7=d1+d2+d3+d4+d5+d6+d7