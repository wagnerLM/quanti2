## Visualização e diagnóstico de variáveis
# instalando pacotes necessários
installpackages("mgm")
install.packages("psych")
# ativando os pacotes
library(mgm)
library(psych)

# banco de dados exemplo
# reconhecer o nível de mensuração de cada variável:

View(autism)
?autism_data
View(autism_names)
# artigo do estudo completo:
# http://sci-hub.tw/10.1177/1362361316660309

# visualização e diagnóstico:
mean(autism$WorkHrs)
sd(autism$WorkHrs)

?summary
summary(autism)
summary(autism$WorkHrs)
?describe
describe(autism$WorkHrs)

?hist
hist(autism$WorkHrs)
hist(autism$WorkHrs,freq = F)
lines(density(autism$WorkHrs),col=2)

# teste de normalidade, a hipótese nula é que a distribuição é normal
shapiro.test(autism$WorkHrs)
# necessário para escolha de testes paramétricos
# ou correspondentes não paramétricos
# em quais situações é importante o diagnóstico:
# https://www.sheffield.ac.uk/polopoly_fs/1.579191!/file/stcp-karadimitriou-normalR.pdf 

#
table(autism$Gen)
prop.table(table(autism$Gen))
table(autism$Gen,autism$ToH)
prop.table(table(autism$Gen,autism$ToH))
prop.table(table(autism$Gen,autism$ToH),1)
prop.table(table(autism$Gen,autism$ToH),2)
?pie
pie(table(autism$Gen,autism$ToH))
pie(table(autism$Gen,autism$ToH),labels = c("1,1","2,1","1,2","2,2"))
?barplot
barplot(table(autism$Gen,autism$ToH))

?boxplot
boxplot(autism$IQ~autism$Gen)
boxplot(autism$SatTreat~autism$Gen)

?plot
plot(autism$SatTreat,autism$NoC)

?pairs.panels
pairs.panels(autism[,c(2,6,7)],jiggle = T,factor = 5)

# Use a função "rowSums", compute os escores das subescalas
# da DASS-21 e descreva suas medidas de tendência central,
# dispersão, e explore graficamente a relação entre elas:

# estresse c(1,6,8,11,12,14,18)
# ansiedade c(2,4,7,9,15,19,20)
# depressao c(3,5,10,13,16,17,21)