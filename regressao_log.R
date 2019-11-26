#######
# Regressão logistica binaria no R
# Envolve um desfecho (VD) binario ou dicotomico 
# Exemplos: morreu/sobrevieu; 
# promovido/nao_promovido; 
# com_depressao/sem_depressao
# Por que usar uma funcao logistica?
# "http://res.cloudinary.com/dyd911kmh/image/upload/f_auto,q_auto:best/v1523361626/linear_vs_logistic_regression_h8voek.jpg"
# As VIs podem ser dicotomicas, ordinais ou contínuas

# Chance e razão de chances (odds ratio) - probabilidades
# caso uma modeda esteja "viciada" para um resultado em 70% cara e 30% coroa
# chance de cara = 70/30 = 2.33
# chance de coroa = 30/70 = .43
# razão de chaces
# (70/30)/(30/70)
# (70*70)/(30*30)
# 5.44 mais chances ou 444% de aumento de probabilidade
# p(X)=(e??0+??1X)/(1+e??0+??1X)
# p(X)/1???p(X) = e??0+??1X # chance
# log(p(X)/1???p(X))=??0+??1X # logaritmo de chance (logit = log odds unit)

# banco envolve variaveis academicas e classificacao de altas habilidades (high abilities)
banco_log<-read.csv("https://raw.githubusercontent.com/wagnerLM/quanti2/master/reglog_ha",sep=";")
View(banco_log)
?glm
glm.fit<-glm(banco_log$ha ~ banco_log$read + banco_log$write + banco_log$math + banco_log$science + banco_log$socst + banco_log$female,
family = binomial(link = "logit"))
summary(glm.fit) # coeficientes (betas) das VIs 
# log(p/1-p)= -46.87 + .25*read + .25*math + .30*science

exp(coef(glm.fit)) # razões de chance
# read + 28%; math + 28% e science + 35% probabilidade do desfecho altas habilidades
glm.prob<-predict(glm.fit,type = "response")
View(table(glm.prob,banco_log$ha))
?ifelse
glm.pred <- ifelse(glm.prob > 0.5, "ha", "no_ha")
table(glm.pred,banco_log$ha)
# Porcentagem da classificacao correta
((162+26)/200)*100
