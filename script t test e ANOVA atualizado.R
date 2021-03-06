#### comparacoes de 2 ou mais grupos

# teste t (2 grupos)
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
round(d,digits = 2)

library(psych)
d_cohen<-cohen.d(bancot$CON,bancot$SEX)
d_cohen$cohen.d
d_cohen$r

# visualize as diferen�as entre grupos por meio do d de Cohen:
# http://rpsychologist.com/d3/cohend/

# a vers�o n�o param�trica do teste t de student � o U de Mann-Whitney para testes independentes
# e Wilcoxon Signed Rank Test para testes dependentes (mesmo grupo)

# use a fun��o wilcox.test e repita as an�lises: 
# investigue diferen�as nos n�veis dos cinco grandes fatores em pessoas que realizam ou n�o trabalho volunt�rio
t.test(bancot$EXT~bancot$SEX)
t.test(bancot$SOC~bancot$SEX)
t.test(bancot$CON~bancot$SEX)
t.test(bancot$NEU~bancot$SEX)
t.test(bancot$ABE~bancot$SEX)

wilcox.test(bancot$EXT~bancot$SEX)
wilcox.test(bancot$SOC~bancot$SEX)
wilcox.test(bancot$CON~bancot$SEX)
wilcox.test(bancot$NEU~bancot$SEX)
wilcox.test(bancot$ABE~bancot$SEX)

### anova (3 ou mais grupos)
# https://rpsychologist.com/d3-one-way-anova 
# banco de dados
anova<-read.csv("https://raw.githubusercontent.com/wagnerLM/quanti2/master/anova_b",sep=";")
View(anova)
# Atividade: 1 = estuda, 2 = trabalha e 3 = estuda E trabalha
# Escolariza��o: 1 = fund/med/tec, 2 = sup e 3 = p�s-grad

# inspe��o gr�fica
boxplot(anova$CON~anova$Escol)
mean(anova[which(anova$Escol==1),]$CON,na.rm = T)
mean(anova[which(anova$Escol==2),]$CON,na.rm = T)
mean(anova[which(anova$Escol==3),]$CON,na.rm = T)
sd(anova[which(anova$Escol==1),]$CON,na.rm = T)
sd(anova[which(anova$Escol==2),]$CON,na.rm = T)
sd(anova[which(anova$Escol==3),]$CON,na.rm = T)

# existe diferen�a significativa? entre quais grupos?

# testar o pressuposto de igualdade de variancias
install.packages("car")
library(car)
leveneTest(anova$CON~as.factor(anova$Escol))

# use a f�rmula para conduzir uma ANOVA 
fit <- aov(y ~ as.factor(A), data=mydataframe)
# modelo:
fit1<-aov(anova$CON~as.factor(anova$Escol))

summary(fit1)
# como reportar: 
# F(2, 485) = 12,65, p < 0.001 

# testes post-hoc para detectar as diferen�as entre os grupos:
?TukeyHSD()
# quais grupos diferem? 
TukeyHSD(fit1)
# para calcular o tamanho de efeito (eta quadrado): 
install.packages("sjstats")
install.packages("effectsize")
library(sjstats)
library(effectsize)

eta_sq(fit1)
cohens_f(fit1)
# eta_sq()       # 0.01 (pequeno), 0.06 (m�dio) e 0.14 (grande)
# cohens_f()     # 0.10 (pequeno), 0.25 (m�dio) e 0.40 (grande)

# n�o parametrico
?kruskal.test
kruskal.test(anova$CON~as.factor(anova$Escol))

# post hoc nao parametrico
?pairwise.wilcox.test

pairwise.wilcox.test(anova$CON,as.factor(anova$Escol),p.adjust.method = "none")
# ajuste do p 
?p.adjust
pairwise.wilcox.test(anova$CON,as.factor(anova$Escol),
                     p.adjust.method = "bonf")

# examine as diferen�as entre os grupos de escolaridade 
# e atividade nas vari�veis do banco "anova"

