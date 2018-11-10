# Case do Econometric Games
library(tidyverse)
library(knitr)
library(kableExtra)

load("dados_econometric_games.RData")


# Questao 1 ---------------------------------------------------------------

# descritivas da base esf_00

descritivas <- coorte_00 %>% 
  filter(matricula != 0) %>% 
  mutate(prop = nv / matricula) %>% 
  select(nv, matricula, prop) %>% 
  gather(key = variavel, value = valor) %>% 
  group_by(variavel) %>% 
  summarise(media = mean(valor), mediana = median(valor), desv_p = sd(valor))  
  
kable(descritivas,
      format = "html",
      digits = 3,
      col.names = c("Est. Desc.", "Média", "Mediana", "Desv. Pad.")) 


  t() %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble()
colnames(descritivas) <- descritivas[1,]  
descritivas <- descritivas[-1,]
# Modelo de regressão
# matricula = b1 + b2*esf + b3*nv
