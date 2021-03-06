# Diagn�stico de itens e pressupostos da An�lise Fatorial Explorat�ria
install.packages("psych")
library(psych)
# Pressupostos, correla��o e diagn�stico de itens (inspe��o gr�fica)
# A an�lise fatorial � uma t�cnica que parte da hip�tese da causa comum, 
# isto �, a correla��o entre itens de um teste ou de subtestes
# � explicada por uma ou mais vari�veis latentes, n�o observ�veis. 
# Dito de outra forma, a an�lise fatorial assume um efeito causal a 
# partir de correla��es entre observ�veis

# Para investigar essas correla��es, vamos observar a distribui��o dos itens:
# use este comando para redefinir o plano de gr�ficos "par(mfrow=c(1,1))"

# carregando os bancos
ESV<-read.csv("https://raw.githubusercontent.com/wagnerLM/SBP/master/ESV.csv",sep=";")
View(ESV)
# Acrescentando labels
ESV_labels<-list("A minha vida est� pr�xima do meu ideal","Minhas condi��es de vida s�o excelentes","Eu estou satisfeito com a minha vida","At� agora eu tenho conseguido as coisas importantes que eu quero na vida"," Se eu pudesse viver a minha vida de novo eu n�o mudaria quase nada")
ESV_labels
View(ESV_labels)
# construa histogramas das vari�veis
hist(ESV[,1])
shapiro.test(ESV[,1])

# observe as correla��es entre os itens
cor.plot(ESV[,-c(6,7)],numbers = TRUE)

# por fim, produza um gr�fico com scatter plot, histogramas e correla��es
pairs.panels(ESV[,-c(6,7)], histogram=TRUE, pch=19)

#KMO
?KMO
KMO(ESV[,-c(6,7)])
# Bartlett
?cortest.bartlett
cortest.bartlett(ESV[,-c(6,7)])

#- Tecnicas de reten��o de fatores (Kaiser, Scree test, VSS, MAP, An�lise paralela)
ESV_eig<-eigen(cor(ESV[,-c(6,7)]))
plot(ESV_eig$values,type="b")

# VSS e MAP
?VSS
ESV_vss<-
  VSS(ESV[,-c(6,7)],cor="poly")
VSS.plot(ESV_vss)

# An�lise paralela
fa.parallel(ESV[,-c(6,7)],cor="poly")

# An�lise fatorial 
fa(ESV[,-c(6,7)],cor="poly",fm="minrank")
# Cargas fatoriais, comunalidade, uniqueness 

# Fidedignidade: alpha e Lambda 6 Guttman
alpha(ESV[,-c(6,7)])

# Modelos multidiensionais
Big5<-read.csv("https://raw.githubusercontent.com/wagnerLM/SBP/master/big5csv",sep=";")
View(Big5)
fa.parallel(Big5[,-c(26,27)],cor="poly")
fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank")

# rota��o obl�qua
fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank",rotate = "oblimin")

# Fidedignidade: alpha e Lambda 6 Guttman
# use o argumento "check.keys = TRUE" no caso de itens invertidos
alpha(Big5[,c(1,6,11,16,21)],check.keys = TRUE)
alpha(Big5[,c(2,7,12,17,22)])
alpha(Big5[,c(3,8,13,18,23)])
alpha(Big5[,c(4,9,14,19,24)])
alpha(Big5[,c(5,10,15,20,25)])

# Escores fatoriais
Big5_fa<-fa(Big5[,-c(26,27)],5,cor="poly",fm="minrank",rotate = "oblimin",scores = "regression")

# Permite usar o escore modelado para investigar associa��es com vari�veis relevantes

# teste t por sexo
t.test(Big5_fa$scores[,1]~Big5$Sexo)
t.test(Big5_fa$scores[,2]~Big5$Sexo)
t.test(Big5_fa$scores[,3]~Big5$Sexo)
t.test(Big5_fa$scores[,4]~Big5$Sexo)
t.test(Big5_fa$scores[,5]~Big5$Sexo)
boxplot(Big5_fa$scores[,5]~Big5$Sexo)

# correla��es com vari�veis externas/crit�rio
plot(Big5_fa$scores[,3]~Big5$Idade)
abline(lm(Big5_fa$scores[,3]~Big5$Idade),col="red")
cor(Big5_fa$scores[,3],Big5$Idade)