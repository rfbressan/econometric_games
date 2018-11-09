##########################################################################################
##################### exemplo de propensity score matching ###############################
##########################################################################################

### o objetivo é parear os individuos de acordo com sua probabilidade de receber o trata-
### mento para então poder ser feito a comparação do efeito do tratamento.

## historinha: Queremos ver qual o efeito de um treinamento recebido pelos trabalhadores
## em seus salários. Para isso utilizaremos a idade, anos de educação e se é casado para
## fazer o pareamento das probabilidades e ent?o medir o efeito do treinamento.

# carregando pacotes necessários
library(MatchIt)
library(Matching)
library(stats)
library(stargazer)
library(readr)

# preparando os dados: temos as variáveis nas colunas e observações nas linhas
# é necess?rio ter uma variavel binaria que "separe" as observações em grupos, 
# por exemplo, no nosso caso é se o indivíduo recebeu ou não o treinamento.
dados <- read_delim(file = paste0(getwd(),"/exemplos de regressão/exemplo - PSM.csv"),
                    delim = ",")

attach(dados)
dados <- dados[,c("TREAT","AGE","EDUC","RE78","MARR")]
head(dados)
tail(dados)
attach(dados)

## agora fazemos o pareamento com o pacote MatchIt
m.out <- matchit(formula = TREAT~AGE+EDUC+MARR,data = dados,method = 'nearest',
                 ratio=1)
sum1 <- summary(m.out) # informações sobre as médias antes e depos do pareamento
plot(m.out,type = 'hist',col='darkgreen') # op??o 'Jitter" mostra a distribui??o 'alinhada'

## salvar dados pareados
# m.data1 <- match.data(m.out)
# write.csv2(m.data1,file = 'outputmatcheddata.csv')

## Calcular o efeito médio sobre os tratados (ATT)

# Estimando probabilidades de receber o tratamento 
prob <- glm(formula = TREAT~AGE+EDUC+MARR,family = binomial(link='probit'),data = dados)
sum2 <- summary(prob) # verificar a interpreta??o dos coeficientes desse summary
fittedprob <- fitted.values(prob)
r1 <- Match(Y = RE78,Tr = TREAT, X = fittedprob,estimand = "ATT") 
sum3 <- summary.Match(r1)

## Tabelas e gráficos para análise dos resultados
# Tamanhos das amostras
stargazer(t(sum1$nn))
# Estatisticas dos tratados e controles antes do pareamento
stargazer(t(sum1$sum.all))
# Estatisticas dos tratados e controles depois do pareamento
stargazer(t(sum1$sum.matched))
# resultado da estimação do efeito médio do tratamento sobre os tratados
summary.Match(r1) # onde interpreta-se que o efeito medio do treinamento recebido pelos
                  # trabalhadores foi uma diminuição de 6681 dolares, além disso esse
                  # resultado foi significativo ao nível de 1%.

# obs: O objeto sum3 está sendo salvo como nulo, o que é estranho. Caso seja utilizado es-
# se modelo colocamos o resultado de ATT estimando em uma tabela MANUALMENTE.


