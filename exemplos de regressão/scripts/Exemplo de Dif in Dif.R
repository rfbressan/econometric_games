########################################################################################################
##################### Exemplo Diferen?as em Diferen?as #################################################
########################################################################################################

## Objetivo: É necessária a hipótese de que o grupo controle e o grupo tratado tem trajetórias
# paralelas, isto é, não é necessário que tenham o mesmo valor em termos de níveis, apenas que a tra-
# jetória da variável de interesse do grupo de controle seja a mesma da do grupo tratdo. O método con-
# siste em calcular a diferença entre o antes e depois da variável de interesse para o grupo que rece-
# beu o tratamento com a diferença entre o antes e depois da variavel de interesse do grupo de contro-
# le.

## Historinha: Temos um painelzinho e vamos estimar o efeito de um tratamento sobre os individuos que 
# identificados como E, F e G e supomos que o tratamento começou em 1994.

### OBS: OS DADOS DEVEM ESTAR EM FORMATO DE PAINEL ###

## pacote necessário
library(foreign)

## Importando e manipulando os dados
dados <- read.dta(file = paste0(getwd(),"/exemplos de regressão/Panel101.dta"))

# Colocando os anos em forma de dummy (1 se é passado o tratamento e 0 se é antes do tratamento)
dados$time <- ifelse(dados$year>=1994,1,0)

# Colando dummy para os paises que receberam o tratamento (1 se receberam e 0 se não receberam)
dados$treated <- ifelse(dados$country=='E'|dados$country=='F'|dados$country=='G',1,0)

# Criando interação entre tempo e tratamento, pois esse é o coeficiente que do o efeito Dif in Dif
dados$did <- dados$treated*dados$time
attach(dados)

## Estimando o modelo 
reg1 <- lm(y~treated+time+did,data = dados)
sum1 <- summary(reg1) # o coeficiente 'did' é o que dá o efeito dif in dif
sum1

# Podemos estimar através do método de multiplicação, não é necessário gerar a intera??o
reg2 <- lm(y~treated*time,data = dados)
sum2 <- summary(reg2)
sum2


# interpretamos o resultado como: em média a participação no tratamento resultou numa diminuição 
# 2.52e+09 na variável de interesse (nesse caso y)