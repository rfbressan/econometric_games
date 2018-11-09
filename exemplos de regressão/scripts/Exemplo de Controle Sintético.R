########################################################################################################
################################ Exemplo de Controle Sintético #########################################
########################################################################################################

## Objetivo é "criar" um individuo sintético (i.e., fictício) que se assemelhe muito ao indivíduo que 
# recebeu o tratamento através de outros indivíduos, que são chamados de controle. Desse modo compara-se
# o período posterior do tratamento do sintético com o "de facto", de modo que tenha-se estimado o efei-
# do tratamento. (Assume-se a Hip?tese de que as tend?ncias s?o identicas)

## Historinha: Aqui vamos examinar o efeito do terrorismo durante a década de 1970 no pais Basco. O ob-
# jetivo é ver se esse, i.e., o terrorismo, afetou o pib per capita do país Basco, para tal são utiliza-
# dados de outras regições 17 regiões da Espanha e diversos indicadores econômicos para a constru??o de um
# pais Basco sintético.

# Pacote Necessario
library(Synth)

############## Manipulação dos dados: 

##### OS DADOS DEVEM ESTAR EM FORMATO DE PAINEL ANTES DE SUA manipulação #######

data('basque')
dataprep.out <- dataprep(foo = basque,
                        predictors = c('school.illit','school.prim', 'school.med', 
                                       'school.high','school.post.high','invest'), 
                        predictors.op = 'mean',                                    
                        time.predictors.prior = 1964:1969,                         
                        special.predictors = list(
                          list('gdpcap',1960:1969,'mean'),
                          list('sec.agriculture',seq(1961,1969,2),'mean'),
                          list('sec.energy',seq(1961,1969,2),'mean'),
                          list('sec.industry',seq(1961,1969,2),'mean'),
                          list('sec.construction',seq(1961,1969,2),'mean'),
                          list('sec.services.venta',seq(1961,1969,2),'mean'),
                          list('sec.services.nonventa',seq(1961,1969,2),'mean'),
                          list('popdens',seq(1961,1969,2),'mean')),
                        dependent = 'gdpcap',
                        unit.variable = 'regionno',
                        unit.names.variable = 'regionname',
                        time.variable = 'year',
                        treatment.identifier = 17,
                        controls.identifier = c(2:16,18),
                        time.optimize.ssr = 1960:1969,
                        time.plot = 1955:1997)

## OBS: argumento "predictors" d? as variaveis com as quais ser?o feitas o controle sint?tico; O "time.predi
# ctors" diz o periodo antes da interven??o que ser? utilizado; O "special.predictors" permite a intro-
# de variaveis com diferentes periodos, visto que ? poss?vel especificar o per?odo no qual ser? calcu-
# lada a m?dia de cada vari?vel.

## A sa?da dessa fun??o fornece entre v?rios objetos as matrizes X0,X1,Z0,Z1 que s?o utlizados para ca-
# cular os pesos de cada regi?o e de cada vari?vel para cada regi?o. O que pode ser ?til para a manipu-
# la??o das variaveis. Por exemplo abaixo transformamos as variaveis de ensino superior e pos gradua??o
# em uma s? e transformamos as variaveis de edu??o em termos percentuais

# Juntando numa s?
dataprep.out$X1['school.high',] <- dataprep.out$X1['school.high',]+
  dataprep.out$X1['school.post.high',]
dataprep.out$X1 <- as.matrix(dataprep.out$X1[
  -which(rownames(dataprep.out$X1)=="school.post.high"),])

dataprep.out$X0["school.high",] <- dataprep.out$X0["school.high",]+
  dataprep.out$X0["school.post.high",]
dataprep.out$X0 <- as.matrix(dataprep.out$X0[
  -which(rownames(dataprep.out$X0)=="school.post.high"),])

# Transformando os valores em percentuais
highest <- which(rownames(dataprep.out$X0)=='school.illit')
lowest <- which(rownames(dataprep.out$X0)=='school.high')


dataprep.out$X1[lowest:highest,] <- 100*(dataprep.out$X1[
  lowest:highest,]/sum(dataprep.out$X1[lowest:highest,]))

dataprep.out$X0[lowest:highest,] <- 100*(dataprep.out$X0[
  lowest:highest,]/sum(dataprep.out$X0[lowest:highest,]))

############## Estimando o Efeito

synth.out <- synth(data.prep.obj = dataprep.out,optimxmethod = 'BFGS')

# O ?nico argumento necess?rio para a estima??o ? o objeto gerado pela fun??o "dataprep", ? interessan-
# te notar que o argumento "optimxmethod" escolhe o m?todo de minimiza??o da dist?ncia entre o real e 
# o sint?tico (BFGS ? o algoritmo quasi newton).

### Avaliando os Resultados
## Tabelas de compara??o (tem tabela com os 'predicted', pesos V e W)
tab <- synth.tab(synth.res = synth.out,dataprep.res = dataprep.out,round.digit = 3)

## Graficos (Tem as op??es de label e legenda imbutidas na fun??o)
# Mostra a trajet?ria (antes e depois do tratamento) da variavel de interesse para o pais Basco e para 
# o controle sint?tico. 
path.plot(synth.res = synth.out,dataprep.res = dataprep.out)
# Mostra a trajet?ria (antes e depois do tratamento) para a diferen?a da variavel de interesse para o 
# entre o pais Basco e o controle
gaps.plot(synth.res = synth.out,dataprep.res = dataprep.out)

############## Teste de Placebo

### Dois tipos de teste de placebo podem ser feito: (i) trocando na estima??o o individuo que recebe o
# tratamento e (ii) trocar o per?odo no qual supostamente ocorreu o tratamento.

