

# Carregando dados e arrumando painel -------------------------------------

library(tibble)
library(dplyr)
library(stargazer)
load("dados_econometric_games.RData")

coorte_00 <- as_tibble(coorte_00)
coorte_05 <- as_tibble(coorte_05)
esf_00 <- as_tibble(esf_00)
esf_05 <- as_tibble(esf_05)
esf_total <- as_tibble(esf_total)


sum(esf_00$esf) - sum(esf_05$esf)

# 2000
esf_00 %>% 
  left_join(coorte_00,
            by = c("idmun", "ano")) -> a2000
       
# 2005
esf_05 %>% 
  left_join(coorte_05,
            by = c("idmun", "ano")) -> a2005

bind_rows(a2000,a2005) %>% 
  mutate(uf = as.factor(uf),
         ano = as.factor(ano)) -> dados_item2

# dif-in-dif --------------------------------------------------------------

dados_item2

## Estimando o modelo 
reg1 <- lm(matricula~esf+nv+ano+uf,data = dados_item2)
sum1 <- summary(reg1) # o coeficiente 'did' é o que dá o efeito dif in dif
sum1





