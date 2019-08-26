#o comando as.numeric(conidção) cria a binária automaticamente.

#No exemplo do exemplo co2=b0+b1*setor2 + b3*alta, uma opção mais simples para construir a binária alta é:
alta=as.numeric(pib>=12000)

#O R automaticamente, a partir da funçao as.numeric() cria a binária.
#Na aula usamos a condição if

#Para o caso do arquivo Dados_PIB.xlsx, temos a vaiável sigla dos países.
#d1=binária se Alemanha, d2 se Canadá, e assim sucessivamente

sigla=Dados_CO2$sigla

d1=as.numeric(sigla=='DEU') #colocamos duas vezes o sinal == pois vamos comparar caracteres
d2=as.numeric(sigla=='CAN')
d3=as.numeric(sigla=='USA')
d4=as.numeric(sigla=='ITA')
d5=as.numeric(sigla=='FRA')
d6=as.numeric(sigla=='JPN')
d7=as.numeric(sigla=='GBR')

binaria_G7=d1+d2+d3+d4+d5+d6+d7