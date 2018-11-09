####################################################################################
####################### Exemplo de regress?o descontinua ###########################
####################################################################################

# Tem-se um ponto a partir da variável (contínua) sobre a qual é feita a seleção em que 
# a probabilidade de participar do tratamento tem um salto. o objetivo desse método ? 
# estimar o tamanho nesse salto.


#### CASO SHARP (Atingindo o ponto Z=z0 o individuo passa a receber o tratamento)


## historinha: Tem-se os dados das eleições para o senado dos estados unidos para um seat
# do partido democratico. A variável 'voto' representa o percentual recebido pelo candidato
# democrata para o senado e a variável 'margin' é a margem com a qual um candidato ao mesmo
# seat ganhou ou perdeu a aleição (portanto variava de -100 a 100). O objetivo ? ver qual
# o efeito de ter sido eleito no resultado das eleições seguintes no ponto de descontinu-
# idade, i.e., em margin=0. 

# Pacotes necessários
library(rdrobust)
library(stargazer)
library(readr)

#importando os dados
dados <- read_delim(file = paste0(getwd(),"/exemplos de regressão/Exemplo Rdd.csv"),
                    delim = ",")
attach(dados)



# Plot da regressão (certa)
(rdplot(y = Vote,
        x = Margin,
        title = 'RD Plot - Senate Elections Data',
        x.label = 'margin (at time t)',
       y.label = 'vote (at time t+1)'))

# Exemplo de placebo, utiliza-se uma sequencia aleatória, mas no estudo deve-se utilizar
# um falso y, i.e., uma variável não deve apresentar a descontinuidade em x=0

ale <- rnorm(1390,mean = 0,sd = 0.5)
(rdplot(y = ale,x = Margin,title = 'RD Plot - Senate Elections Data',x.label = 'margin (at time t)',
        y.label = 'vote (at time t+1)'))

# construção do gráfico com as janelas de IMSE (Integrated Mean Square Error)
(rdplot(y=Vote,x=Margin,title = 'RD Plot - Senate Elections Data',x.label = 'margin (at time t)',
        y.label = 'vote (at time t+1)',binselect = 'es',scale = 5))

# Estimando O ATT, i.e., o efeito do tratamento (nesse caso ter ganho a eleição) na 
# descontinuidade.
rd <- rdrobust(y=Vote,x=Margin,kernel = 'uniform')
sum <- summary(rd) # aqui interpretamos os coeficientes da seguinte maneira: Em média o 
                   # candidato do partido republicano que foi eleito na margem recebe 7%
                   # a mais de votos na eleição seguinte em comparação com o candidato que
                   # perdeu a eleição na margem.

# Estudar o objeto rd para entender o que ele indica