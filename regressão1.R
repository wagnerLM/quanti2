#### Regressão linear, regressão linear múltipla, 
#### seleção de variáveis e regressão não paramétrica:

# Regressão linear tem por objetivo predizer valores de uma 
# variável a partir de outra. Predizer não é o mesmo que causar,
# é tentar inferir um valor a partir de outro. A relação de 
# causalidade (temporal) não é possível de inferir nesta análise.

# Instalando os pacotes necessários
install.packages("lm.beta")
library(lm.beta)
install.packages("car")
library(car)

# Carregando o banco:
banco_rev<-read.csv(file.choose(),sep=";")

# Análise de correlação e reta de aderência
cor(banco_rev$SV,banco_rev$AU)
plot(banco_rev$SV,banco_rev$AU)
abline(lm(banco_rev$SV~banco_rev$AU), col="red")

# Perceba que ao descrever a relação linear entre ambas 
# as variáveis, utiliza-se uma reta.
# Sua representação é y = b + ax + E
# Em que "y" é a variável sendo predita (variável dependente), 
# "b" é o intercepto (valor de y quando x = 0), 
# "a" é a inclinação da reta ou coeficiente angular, 
# "x" é o valor observado na variável preditora (variável independente),
# e "E" é o erro associado. 

# Utilizando a função "lm" (linear model),estime
# os esses parâmetros, crie um modelo para 
# predizer satisfação com a vida (SV) por meio de autonomia (AU):

reg1 <- lm(banco_rev$SV ~ banco_rev$AU, data=banco_rev)

summary(reg1)  # resumo do modelo
coefficients(reg1) # coeficientes não padronizados 
View(fitted(reg1)) # valores preditos
View(residuals(reg1)) # resíduos
View(cbind(banco_rev$SV,fitted(reg1),residuals(reg1))) #como visualizar valores originais, preditos e resíduos
plot(reg1,1) # visualizando resíduos
outlierTest(reg1) # detecção de outliers
ncvTest(reg1) # teste de homocedasticidade dos resíduos

# A partir do banco com os valores de SV, AU, valor predito e resíduo
# e dos parâmetros da regressão, monte a fórmula de regressão:
View(cbind(banco_rev$SV,fitted(reg1),residuals(reg1),banco_rev$AU))
y = b + ax + E

# O coeficiente de regressão informado pela função lm é
# o "b", um parâmetro **amostral** 
# Para obter o parâmetro **populacional** ou beta é necessário
# padronizar (z score) as variáveis dependente e independente:
# usando a função "scale"
SVz<-scale(banco_rev$SV,center=T,scale=T)
AUz<-scale(banco_rev$AU,center=T,scale=T)
cor(SVz,AUz)
reg1z<-lm(SVz~AUz)
summary(reg1z)
# ou a função "lm.beta"
lm.beta(reg1)

# Crie um modelo com as variáveis que mais se relacionam com SV
View(banco_rev)
cor(banco_rev[,-c(1)]) 

# Trata-se agora de um modelo de regressão linear múltipla, expresso por:
#  y = b + a1x1 + a2x2 + ... + E
# "a1" e "a2" são os coeficientes angulares das variáveis x1 e x2 do modelo
# são estimados por meio de correlações semi-parciais com y, ou a VD
reg2<- lm(banco_rev$SV ~ banco_rev$AA + banco_rev$Dep , data=banco_rev)
summary(reg2)
lm.beta(reg2)
coefficients(reg2) # model coefficients
confint(reg2, level=0.95) # CIs for model parameters 
View(fitted(reg2)) # predicted values
View(residuals(reg2)) # residuals
View(cbind(banco_rev$SV,fitted(reg2),residuals(reg2)))
anova(reg2) # anova table 
vcov(reg2) # covariance matrix for model parameters 

anova(reg1,reg2)

reg3<- lm(banco_rev$SV ~ banco_rev$AA + banco_rev$Dep + banco_rev$AU , data=banco_rev)
summary(reg3)
lm.beta(reg3)
anova(reg2,reg3)

reg4<- lm(banco_rev$SV ~ banco_rev$AA + banco_rev$Dep + banco_rev$Ans , data=banco_rev)
summary(reg4)
lm.beta(reg4)
anova(reg2,reg4)
cor(banco_rev$Ans,banco_rev$Dep)

### Método "enter"
reg5<- lm(banco_rev$SV ~ banco_rev$AA + banco_rev$Dep + banco_rev$AU + banco_rev$Ans , data=banco_rev)
summary(reg5)
lm.beta(reg5)
anova(reg2,reg5)
