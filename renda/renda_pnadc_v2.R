library(PNADcIBGE)
library(tidyverse)
library(srvyr)

# Preâmbulo
ano <- 2022
trimestre <- 2
minimo <- 1212


pnadc <- get_pnadc(year = ano, quarter = trimestre, vars = c("V2009", "VD4019", "VD4020"))

pnadc <- as_survey(pnadc)

# VD4019: Renda habitual; VD4020: Renda efetiva
aux <- pnadc$variables %>% 
    group_by(ID_DOMICILIO) %>% 
    summarise(renda_dmcl=sum(VD4020, na.rm = T))

pnadc$variables <- pnadc$variables %>% 
    left_join(aux) %>% 
    mutate(faixa_dmcl=cut(renda_dmcl,
                          breaks=c(-Inf, minimo*2, minimo*3, minimo*5, minimo*10, minimo*20, minimo*50, Inf),
                          labels=c("0 a 2", "2 a 3", "3 a 5", "5 a 10", "10 a 20", "20 a 50", ">50")))

resumo <- pnadc %>% 
    filter(V2009>=16) %>% 
    group_by(faixa_dmcl) %>% 
    summarise(x=survey_mean())

print(resumo)
