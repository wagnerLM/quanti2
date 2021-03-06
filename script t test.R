?t.test
bancot<-read.csv("https://raw.githubusercontent.com/wagnerLM/quanti2/master/banco_t",sep=";")
View(bancot)

# verificar as m�dias nos cinco grandes fatores da personalidade
boxplot(bancot[,c(1:5)])

# verificar as m�dias por sexo
boxplot(bancot$EXT~bancot$SEX)
boxplot(bancot$SOC~bancot$SEX)
boxplot(bancot$CON~bancot$SEX)
boxplot(bancot$NEU~bancot$SEX)
boxplot(bancot$ABE~bancot$SEX)

# testar a signific�ncia estat�stica dessas diferen�as
# o teste t de student assume como hip�tese nula que as duas m�dias pertencem � mesma popula��o
# podemos testar de duas formas 

# se as m�dias s�o provenientes do mesmo grupo, temos um teste t pareado
t.test(bancot$CON,bancot$NEU,paired = T)
boxplot(bancot[,c(3,4)])

# se as m�dias s�o provenientes de grupos diferentes, temos um teste t independente
t.test(bancot$CON~bancot$SEX)
boxplot(bancot$CON~bancot$SEX)

# al�m da signific�ncia estat�stica da diferen�a observada, podemos estimar o tamanho de efeito
# o mais conhecido � o d de Cohen 
# d = (m1 - m2)/sqrt((sd1�+sd2�)/2)
d<-(mean(bancot[which(bancot$SEX == 1),]$CON,na.rm = T)-mean(bancot[which(bancot$SEX == 2),]$CON,na.rm = T))/ sqrt(((sd(bancot[which(bancot$SEX == 1),]$CON,na.rm = T)^2) + (sd(bancot[which(bancot$SEX == 2),]$CON,na.rm = T)^2))/2)
d
# visualize as diferen�as entre grupos por meio do d de Cohen:
# http://rpsychologist.com/d3/cohend/

# a vers�o n�o param�trica do teste t de student � o U de Mann-Whitney para testes independentes
# e Wilcoxon Signed Rank Test para testes dependentes (mesmo grupo)

# use a fun��o wilcox.test e repita as an�lises: 
# investigue diferen�as nos n�veis dos cinco grandes fatores em pessoas que realizam ou n�o trabalho volunt�rio