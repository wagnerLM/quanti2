### ANOVA
anova<-read.csv("https://raw.githubusercontent.com/wagnerLM/quanti2/master/anova_banco",sep=";")
View(anova)

# renomeando labels
anova$Ativ[which(anova$Ativ==1)]<-"estuda"
anova$Ativ[which(anova$Ativ==2)]<-"trabalha"
anova$Ativ[which(anova$Ativ==3)]<-"estudaetrabalha"
View(anova)

#visualização
boxplot(anova$DASSD~anova$Ativ)
boxplot(anova$BEP~anova$Ativ)
boxplot(anova$CON~anova$Ativ)

# análise de variância ou anova
# estatítica F, que é a razão var(entre grupos)/var(intra grupos)
anova_fit<-aov(anova$CON~anova$Ativ)
summary(anova_fit)

# testes post hoc
TukeyHSD(anova_fit)

# tamanho de efeito (eta²)
# SS(efeito)/SS(total) = eta²
133/5419
#interpretação
# .01 pequeno; .06 moderado; .14 grande

# não paramétrico
kruskal.test(anova$CON~anova$Ativ)

# comparações par a par
wilcox.test(anova$CON[which(anova$Ativ!="estuda")]~anova$Ativ[which(anova$Ativ!="estuda")])
wilcox.test(anova$CON[which(anova$Ativ!="trabalha")]~anova$Ativ[which(anova$Ativ!="trabalha")])
wilcox.test(anova$CON[which(anova$Ativ!="estudaetrabalha")]~anova$Ativ[which(anova$Ativ!="estudaetrabalha")])

# Exercício
# Renomeie os labels da variável "Escol" conforme:
# 1 = medio, 2 = superior e 3 = posgrad
# Escolha três variáveis e reporte as análises de ANOVA e Kruskal-Wallis
