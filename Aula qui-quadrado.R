#### QUI-QUADRADO (CHI-SQUARE) ####
# Por Marcela Sanseverino
# Revis�o Wagner Machado

# Ler banco de dados - sintomas psicodermatose e depressivos

data_psiderma <- read.csv(file.choose(), dec = ",", sep = ";", header = TRUE)
View(data_psiderma)

#### Descritivas ####

# Objetos com as preval�ncia dos sintomas de psicodermatose:
# usando as fun��es "prop.table" e "table"
### legenda: 1 - tem ; 0 - n�o tem

prev_roer <- prop.table(table(data_psiderma$Roer))*100
prev_roer
prev_insetos <- prop.table(table(data_psiderma$Inseto))*100
prev_insetos
prev_suor <- prop.table(table(data_psiderma$Suor_excessivo))*100
prev_suor

### Esses foram apenas alguns exemplos, fa�a outros se quiser conhecer
## a amostra que estamos estudando.

### Agora para ter uma ideia geral de quem apresentou pelo menos um sintoma.
## nova legenda -> 0 = nenhum ; 1 = pelo menos 1 sintoma
prev_psiderma <- prop.table(table(data_psiderma$Presen�a_derma))*100
prev_psiderma

### nova legenda -> 0 = sem indica��o para rastreio; 1 = tem idica��o
prev_rastreiodepre <- prop.table(table(data_psiderma$Indica��o_Depre))*100
prev_rastreiodepre

#### QUI-QUADRADO ####
# Teste de ajuste a uma distribui��o te�rica/hipotetica 
# Roer tem uma frequencia de 
freq_roer <- table(data_psiderma$Roer)
freq_roer
# Se n�o houvesse tend�ncia na popula��o, qual a prevalencia esperada?
# R: 50%/50% ou freq = 171/171
roer_h0<-c("0"=171,"1"=171)
# res�duo
freq_roer - roer_h0
# res�duo quadr�tico
(freq_roer - roer_h0)^2
# Desvios considerando o esperado
(freq_roer - roer_h0)^2/roer_h0
# Medida de ajuste / qui-quadrado 
sum((freq_roer - roer_h0)^2/roer_h0 )

# Este valor ter� uma distribui��o e uma probabilidade associada
# Ver: https://learningstatisticswithr-bookdown.netlify.com/navarro2_files/figure-html/goftest-1.png
install.packages("lsr")
library(lsr)
goodnessOfFitTest(as.factor(data_psiderma$Roer))

# Teste de independ�ncia
## Para fazer o chi-quadrado vamos utilizar os pacotes "gmodels", "rcompanion"

install.packages("gmodels")
install.packages("rcompanion")
library(gmodels)
library(rcompanion)

## fun��o para o chi-quadrado e montar a tabela 2x2: CrossTable

## ATEN��O!! 
### Fisher -> quando esperamos que em um dos quadrantes haver� MENOS
## de 5 sujeitos/observa��es
### Chi-Quadrado -> para todas as outras situa��es
## Ao construir a fun��o temos que indicar qual teste queremos utilizar
## se queremos ver os valores esperados, os valores residuais, entre outros
## sempre colocando operador l�gico

## para verificar todos utilizar:
?CrossTable

## Utilizar a fun��o construindo novo objeto:
depre_derma <- CrossTable(data_psiderma$Presen�a_derma, data_psiderma$Indica��o_Depre, digits = 2, expected = TRUE, chisq = TRUE, fisher = FALSE, asresid = TRUE,sresid = TRUE)
depre_etapa <- CrossTable(data_psiderma$Etapa, data_psiderma$Indica��o_Depre, digits = 2, expected = TRUE, chisq = TRUE, fisher = FALSE, asresid = TRUE, sresid = TRUE)

## valores dos res�duos padronizados, indica efeito >= |2|
depre_derma$chisq$stdres
depre_etapa$chisq$stdres

#### Efeito do teste ####

## coeficiente de PHI / v de cramer
## decis�o: phi = tabela 2x2; v de cramer = n�o quadr�tica

?phi
?cramerV
efeito_derma <- phi(data_psiderma$Presen�a_derma, data_psiderma$Indica��o_Depre, ci = TRUE, conf = TRUE)
efeito_derma
efeito_etapa <- cramerV(data_psiderma$Etapa, data_psiderma$Indica��o_Depre, ci = TRUE, conf = TRUE)
efeito_etapa

#### VISUALIZA��O GR�FICA ####

## Pacotes a serem utilizados: "ggplot2"
install.packages("ggplot2")
library(ggplot2)

data_psiderma$Etapa <- as.factor(data_psiderma$Etapa)

?qplot

## Gr�fico de preval�ncia de teste positivo para rastreio de depress�o:
qplot(Indica��o_Depre, data = data_psiderma, geom = "bar")
## Gr�fico de preval�ncia de pelo menos um sintoma de psicodermatose:
qplot(Presen�a_derma, data = data_psiderma, geom = "bar", fill = I("red"))
## Juntando os dois gr�ficos:
qplot(Indica��o_Depre, data = data_psiderma, geom = "bar", facets = Presen�a_derma ~ ., fill = I("purple"))
## Com a l�gica anterior, mas separando a indica��o para rastreio de depress�o
# por etapa do curso:
qplot(Indica��o_Depre, data = data_psiderma, geom = "bar", facets = Etapa ~ ., fill = I("gold"))

?ggplot

# Presen�a_derma x Indica��o_Depre
par(mfrow=c(2,2))

f1<-ggplot(data_psiderma,aes(Indica��o_Depre)) +
  geom_bar(aes(fill=as.factor(Presen�a_derma)))
f1

f2<-ggplot(data_psiderma,aes(Presen�a_derma)) +
  geom_bar(aes(fill=as.factor(Indica��o_Depre)))
f2

f3<-ggplot(data_psiderma,aes(Etapa)) +
  geom_bar(aes(fill=as.factor(Indica��o_Depre)))
f3

f4<-ggplot(data_psiderma,aes(Indica��o_Depre)) +
  geom_bar(aes(fill=as.factor(Etapa)))
f4