### ANOVA
anova<-read.csv("https://raw.githubusercontent.com/wagnerLM/quanti2/master/anova_banco",sep=";")
View(anova)

# renomeando labels
anova$Ativ[which(anova$Ativ==1)]<-"estuda"
anova$Ativ[which(anova$Ativ==2)]<-"trabalha"
anova$Ativ[which(anova$Ativ==3)]<-"estudaetrabalha"
View(anova)

#visualiza��o
boxplot(anova$DASSD~anova$Ativ)
boxplot(anova$BEP~anova$Ativ)
boxplot(anova$CON~anova$Ativ)

# an�lise de vari�ncia ou anova
# estat�tica F, que � a raz�o var(entre grupos)/var(intra grupos)
anova_fit<-aov(anova$CON~anova$Ativ)
summary(anova_fit)

# testes post hoc
TukeyHSD(anova_fit)

# tamanho de efeito (eta�)
# SS(efeito)/SS(total) = eta�
133/5419
#interpreta��o
# .01 pequeno; .06 moderado; .14 grande

# n�o param�trico
kruskal.test(anova$CON~anova$Ativ)

# compara��es par a par
wilcox.test(anova$CON[which(anova$Ativ!="estuda")]~anova$Ativ[which(anova$Ativ!="estuda")])
wilcox.test(anova$CON[which(anova$Ativ!="trabalha")]~anova$Ativ[which(anova$Ativ!="trabalha")])
wilcox.test(anova$CON[which(anova$Ativ!="estudaetrabalha")]~anova$Ativ[which(anova$Ativ!="estudaetrabalha")])

# Exerc�cio
# Renomeie os labels da vari�vel "Escol" conforme:
# 1 = medio, 2 = superior e 3 = posgrad
# Escolha tr�s vari�veis e reporte as an�lises de ANOVA e Kruskal-Wallis
