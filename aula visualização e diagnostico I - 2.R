## Visualização e diagnóstico de variáveis
# instalando pacotes necessários
installpackages("mgm")
install.packages("psych")
# ativando os pacotes
library(mgm)
library(psych)

# banco de dados exemplo
# reconhecer o nível de mensuração de cada variável:

View(autism_data$data)
?autism_data
colnames(autism_data$data)
# artigo do estudo completo:
# http://sci-hub.tw/10.1177/1362361316660309

# visualização e diagnóstico:
mean(autism_data$data$Workinghours)
sd(autism_data$data$Workinghours)

?summary
summary(autism_data$data)
summary(autism_data$data$Workinghours)
?describe
describe(autism_data$data$Workinghours)

?hist
hist(autism_data$data$Workinghours)
hist(autism_data$data$Workinghours,col="red",border="blue")
hist(autism_data$data$Workinghours,col="red",border="blue",main = "my first histogram in R :)")
# densidade (prob)
hist(autism_data$data$Workinghours,freq = F)
lines(density(autism_data$data$Workinghours),col=2)

# teste de normalidade, a hipótese nula é que a distribuição é normal
shapiro.test(autism_data$data$Workinghours)
# necessário para escolha de testes paramétricos
# ou correspondentes não paramétricos
# em quais situações é importante o diagnóstico:
# https://www.sheffield.ac.uk/polopoly_fs/1.579191!/file/stcp-karadimitriou-normalR.pdf 

#
table(autism_data$data$Gender)
prop.table(table(autism_data$data$Gender))
# recodificando

autism_data$data$Gender_rec<-autism_data$data$Gender
autism_data$data$Gender_rec[autism_data$data$Gender_rec==1]<-"Male"
autism_data$data$Gender_rec[autism_data$data$Gender_rec==2]<-"Female"
table(autism_data$data$Gender_rec)
prop.table(table(autism_data$data$Gender_rec))

table(autism_data$data$Gender,autism_data$data$`Type of Housing`)
autism_data$data$`Type of Housing_rec`<-autism_data$data$`Type of Housing`
autism_data$data$`Type of Housing_rec`[autism_data$data$`Type of Housing_rec`==1]<-"House"
autism_data$data$`Type of Housing_rec`[autism_data$data$`Type of Housing_rec`==2]<-"Inst"

prop.table(table(autism_data$data$Gender_rec,autism_data$data$`Type of Housing_rec`))
prop.table(table(autism_data$data$Gender_rec,autism_data$data$`Type of Housing_rec`),1)
prop.table(table(autism_data$data$Gender_rec,autism_data$data$`Type of Housing_rec`),2)


?pie
pie(table(autism_data$data$Gender,autism_data$data$`Type of Housing`))
pie(table(autism_data$data$Gender,autism_data$data$`Type of Housing`),labels = c("1,1","2,1","1,2","2,2"))
lbs<-c("1,1","2,1","1,2","2,2")
pie(table(autism_data$data$Gender,autism_data$data$`Type of Housing`),labels = c("1,1","2,1","1,2","2,2"),col = rainbow(length(lbs)))

?barplot
barplot(table(autism_data$data$Gender,autism_data$data$`Type of Housing`))

?boxplot
boxplot(autism_data$data$IQ~autism_data$data$Gender)
boxplot(autism_data$data$`Satisfaction: Treatment`~autism_data$data$Gender)
boxplot(autism_data$data$`Satisfaction: Treatment`~autism_data$data$Gender_rec)

?plot
plot(autism_data$data$`Satisfaction: Treatment`,autism_data$data$`No of Comorbidities`)
abline(lm(autism_data$data$`Satisfaction: Treatment`~autism_data$data$`No of Comorbidities`),col="red")

?pairs.panels
pairs.panels(autism_data$data[,c(2,6,7)],jiggle = T,factor = 5)

#################################

# Use a função "rowSums" e o operador de indexação "$" e compute 
# os escores das subescalas da DASS-21 e descreva suas medidas de
# tendência central, dispersão, e explore graficamente a relação 
# entre elas:


# estresse c(1,6,8,11,12,14,18)
# ansiedade c(2,4,7,9,15,19,20)
# depressao c(3,5,10,13,16,17,21)

dasspoly<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasspoly",sep = ";")
View(dasspoly)
# Nome resumido dos itens
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames
