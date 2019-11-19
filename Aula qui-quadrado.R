#### QUI-QUADRADO (CHI-SQUARE) ####
# Por Marcela Sanseverino
# Revisão Wagner Machado

# Ler banco de dados - sintomas psicodermatose e depressivos

data_psiderma <- read.csv(file.choose(), dec = ",", sep = ";", header = TRUE)
View(data_psiderma)

#### Descritivas ####

# Objetos com as prevalência dos sintomas de psicodermatose:
# usando as funções "prop.table" e "table"
### legenda: 1 - tem ; 0 - não tem

prev_roer <- prop.table(table(data_psiderma$Roer))*100
prev_roer
prev_insetos <- prop.table(table(data_psiderma$Inseto))*100
prev_insetos
prev_suor <- prop.table(table(data_psiderma$Suor_excessivo))*100
prev_suor

### Esses foram apenas alguns exemplos, faça outros se quiser conhecer
## a amostra que estamos estudando.

### Agora para ter uma ideia geral de quem apresentou pelo menos um sintoma.
## nova legenda -> 0 = nenhum ; 1 = pelo menos 1 sintoma
prev_psiderma <- prop.table(table(data_psiderma$Presença_derma))*100
prev_psiderma

### nova legenda -> 0 = sem indicação para rastreio; 1 = tem idicação
prev_rastreiodepre <- prop.table(table(data_psiderma$Indicação_Depre))*100
prev_rastreiodepre

#### QUI-QUADRADO ####
# Teste de ajuste a uma distribuição teórica/hipotetica 
# Roer tem uma frequencia de 
freq_roer <- table(data_psiderma$Roer)
freq_roer
# Se não houvesse tendência na população, qual a prevalencia esperada?
# R: 50%/50% ou freq = 171/171
roer_h0<-c("0"=171,"1"=171)
# resíduo
freq_roer - roer_h0
# resíduo quadrático
(freq_roer - roer_h0)^2
# Desvios considerando o esperado
(freq_roer - roer_h0)^2/roer_h0
# Medida de ajuste / qui-quadrado 
sum((freq_roer - roer_h0)^2/roer_h0 )

# Este valor terá uma distribuição e uma probabilidade associada
# Ver: https://learningstatisticswithr-bookdown.netlify.com/navarro2_files/figure-html/goftest-1.png
install.packages("lsr")
library(lsr)
goodnessOfFitTest(as.factor(data_psiderma$Roer))

# Teste de independência
## Para fazer o chi-quadrado vamos utilizar os pacotes "gmodels", "rcompanion"

install.packages("gmodels")
install.packages("rcompanion")
library(gmodels)
library(rcompanion)

## função para o chi-quadrado e montar a tabela 2x2: CrossTable

## ATENÇÃO!! 
### Fisher -> quando esperamos que em um dos quadrantes haverá MENOS
## de 5 sujeitos/observações
### Chi-Quadrado -> para todas as outras situações
## Ao construir a função temos que indicar qual teste queremos utilizar
## se queremos ver os valores esperados, os valores residuais, entre outros
## sempre colocando operador lógico

## para verificar todos utilizar:
?CrossTable

## Utilizar a função construindo novo objeto:
depre_derma <- CrossTable(data_psiderma$Presença_derma, data_psiderma$Indicação_Depre, digits = 2, expected = TRUE, chisq = TRUE, fisher = FALSE, asresid = TRUE,sresid = TRUE)
depre_etapa <- CrossTable(data_psiderma$Etapa, data_psiderma$Indicação_Depre, digits = 2, expected = TRUE, chisq = TRUE, fisher = FALSE, asresid = TRUE, sresid = TRUE)

## valores dos resíduos padronizados, indica efeito >= |2|
depre_derma$chisq$stdres
depre_etapa$chisq$stdres

#### Efeito do teste ####

## coeficiente de PHI / v de cramer
## decisão: phi = tabela 2x2; v de cramer = não quadrática

?phi
?cramerV
efeito_derma <- phi(data_psiderma$Presença_derma, data_psiderma$Indicação_Depre, ci = TRUE, conf = TRUE)
efeito_derma
efeito_etapa <- cramerV(data_psiderma$Etapa, data_psiderma$Indicação_Depre, ci = TRUE, conf = TRUE)
efeito_etapa

#### VISUALIZAÇÃO GRÁFICA ####

## Pacotes a serem utilizados: "ggplot2"
install.packages("ggplot2")
library(ggplot2)

data_psiderma$Etapa <- as.factor(data_psiderma$Etapa)

?qplot

## Gráfico de prevalência de teste positivo para rastreio de depressão:
qplot(Indicação_Depre, data = data_psiderma, geom = "bar")
## Gráfico de prevalência de pelo menos um sintoma de psicodermatose:
qplot(Presença_derma, data = data_psiderma, geom = "bar", fill = I("red"))
## Juntando os dois gráficos:
qplot(Indicação_Depre, data = data_psiderma, geom = "bar", facets = Presença_derma ~ ., fill = I("purple"))
## Com a lógica anterior, mas separando a indicação para rastreio de depressão
# por etapa do curso:
qplot(Indicação_Depre, data = data_psiderma, geom = "bar", facets = Etapa ~ ., fill = I("gold"))

?ggplot

# Presença_derma x Indicação_Depre
par(mfrow=c(2,2))

f1<-ggplot(data_psiderma,aes(Indicação_Depre)) +
  geom_bar(aes(fill=as.factor(Presença_derma)))
f1

f2<-ggplot(data_psiderma,aes(Presença_derma)) +
  geom_bar(aes(fill=as.factor(Indicação_Depre)))
f2

f3<-ggplot(data_psiderma,aes(Etapa)) +
  geom_bar(aes(fill=as.factor(Indicação_Depre)))
f3

f4<-ggplot(data_psiderma,aes(Indicação_Depre)) +
  geom_bar(aes(fill=as.factor(Etapa)))
f4