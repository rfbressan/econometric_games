library(tidyr)

modelo_4 <- readRDS("~/Documents/Projetos R/econometric_games/modelo_4.RDS") %>% 
  as_tibble()



modelo_4 %>% 
  filter(ano %in% c(2000, 2005)) %>% 
  gather(key = "matriculas",
         value = "matricula",
         c(matricula_10, matricula_15)) %>%  
gather(key = "nnn",
       value = "nv",
       c(nv_00,nv_05)) %>% 
  select(-c(matriculas,nnn)) %>% 
  mutate(uf = as.factor(uf),
         ano = as.factor(ano)) -> modelo_4
  
reg1 <- lm(matricula~esf99+esf04+nv+uf+ano,data = modelo_4)


