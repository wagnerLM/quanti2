?t.test
bancot<-read.csv("https://raw.githubusercontent.com/wagnerLM/quanti2/master/banco_t",sep=";")
View(bancot)

# verificar as médias nos cinco grandes fatores da personalidade
boxplot(bancot[,c(1:5)])

# verificar as médias por sexo
boxplot(bancot$EXT~bancot$SEX)
boxplot(bancot$SOC~bancot$SEX)
boxplot(bancot$CON~bancot$SEX)
boxplot(bancot$NEU~bancot$SEX)
boxplot(bancot$ABE~bancot$SEX)

# testar a significância estatística dessas diferenças
# o teste t de student assume como hipótese nula que as duas médias pertencem à mesma população
# podemos testar de duas formas 

# se as médias são provenientes do mesmo grupo, temos um teste t pareado
t.test(bancot$CON,bancot$NEU,paired = T)
boxplot(bancot[,c(3,4)])

# se as médias são provenientes de grupos diferentes, temos um teste t independente
t.test(bancot$CON~bancot$SEX)
boxplot(bancot$CON~bancot$SEX)

# além da significância estatística da diferença observada, podemos estimar o tamanho de efeito
# o mais conhecido é o d de Cohen 
# d = (m1 - m2)/sqrt((sd1²+sd2²)/2)
d<-(mean(bancot[which(bancot$SEX == 1),]$CON,na.rm = T)-mean(bancot[which(bancot$SEX == 2),]$CON,na.rm = T))/ sqrt(((sd(bancot[which(bancot$SEX == 1),]$CON,na.rm = T)^2) + (sd(bancot[which(bancot$SEX == 2),]$CON,na.rm = T)^2))/2)
d
# visualize as diferenças entre grupos por meio do d de Cohen:
# http://rpsychologist.com/d3/cohend/

# a versão não paramétrica do teste t de student é o U de Mann-Whitney para testes independentes
# e Wilcoxon Signed Rank Test para testes dependentes (mesmo grupo)

# use a função wilcox.test e repita as análises: 
# investigue diferenças nos níveis dos cinco grandes fatores em pessoas que realizam ou não trabalho voluntário