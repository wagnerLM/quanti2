#### comparacoes de 2 ou mais grupos

# teste t (2 grupos)
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
round(d,digits = 2)

library(psych)
d_cohen<-cohen.d(bancot$CON,bancot$SEX)
d_cohen$cohen.d
d_cohen$r

# visualize as diferenças entre grupos por meio do d de Cohen:
# http://rpsychologist.com/d3/cohend/

# a versão não paramétrica do teste t de student é o U de Mann-Whitney para testes independentes
# e Wilcoxon Signed Rank Test para testes dependentes (mesmo grupo)

# use a função wilcox.test e repita as análises: 
# investigue diferenças nos níveis dos cinco grandes fatores em pessoas que realizam ou não trabalho voluntário
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
# Escolarização: 1 = fund/med/tec, 2 = sup e 3 = pós-grad

# inspeção gráfica
boxplot(anova$CON~anova$Escol)
mean(anova[which(anova$Escol==1),]$CON,na.rm = T)
mean(anova[which(anova$Escol==2),]$CON,na.rm = T)
mean(anova[which(anova$Escol==3),]$CON,na.rm = T)
sd(anova[which(anova$Escol==1),]$CON,na.rm = T)
sd(anova[which(anova$Escol==2),]$CON,na.rm = T)
sd(anova[which(anova$Escol==3),]$CON,na.rm = T)

# existe diferença significativa? entre quais grupos?

# testar o pressuposto de igualdade de variancias
install.packages("car")
library(car)
leveneTest(anova$CON~as.factor(anova$Escol))

# use a fórmula para conduzir uma ANOVA 
fit <- aov(y ~ as.factor(A), data=mydataframe)
# modelo:
fit1<-aov(anova$CON~as.factor(anova$Escol))

summary(fit1)
# como reportar: 
# F(2, 485) = 12,65, p < 0.001 

# testes post-hoc para detectar as diferenças entre os grupos:
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
# eta_sq()       # 0.01 (pequeno), 0.06 (médio) e 0.14 (grande)
# cohens_f()     # 0.10 (pequeno), 0.25 (médio) e 0.40 (grande)

# não parametrico
?kruskal.test
kruskal.test(anova$CON~as.factor(anova$Escol))

# post hoc nao parametrico
?pairwise.wilcox.test

pairwise.wilcox.test(anova$CON,as.factor(anova$Escol),p.adjust.method = "none")
# ajuste do p 
?p.adjust
pairwise.wilcox.test(anova$CON,as.factor(anova$Escol),
                     p.adjust.method = "bonf")

# examine as diferenças entre os grupos de escolaridade 
# e atividade nas variáveis do banco "anova"

