# DISCLAIMER
# Este scripr é parte de uma análise exploratória criada para ajudar a sanar minhas dúvidas a respeito da PNAD Contínua e sua relação com a amostragem em pesquisas eleitorais. 
# Eu não sou especialista nas bases do IBGE nem em pesquisas eleitorais. Qualquer análise decorrente deste script deve ser lida com ceticismo. 


library(PNADcIBGE)
library(tidyverse)
library(srvyr)

# preâmbulo
ano <- 2022
trimestre <- 2
minimo <- 1100
t0 <- Sys.time()

# carrega dados da PNAD da API do IBGE
pnadc <- get_pnadc(year = ano, quarter = trimestre, vars = c("V1008", "V2009", "VD4001","VD4019", "VD4020"))

# transforma os dados no formato do pacote srvyr
pnadc <- as_survey(pnadc)

# cria objeto auxiliar, com o valor de todos os rendimentos dos indivíduos agrupados por domicílio
aux <- pnadc$variables %>% mutate(conc=paste0(UPA, V1008)) %>% group_by(conc) %>% summarise(renda_dmcl=sum(VD4020, na.rm = T))

# cria a chave de relação com a base aux
pnadc$variables <- pnadc$variables %>% mutate(conc=paste0(UPA, V1008))

# left join
pnadc$variables <- pnadc$variables %>% left_join(aux)
    
# categoriza a renda domiciliar nas faixas relativas ao salário mínimo
pnadc$variables <- pnadc$variables %>%
    mutate(faixa_dmcl=cut(renda_dmcl,
                          breaks=c(-Inf, minimo*2, minimo*3, minimo*5, minimo*10, minimo*20, minimo*50, Inf),
                          labels=c("0 a 2", "2 a 3", "3 a 5", "5 a 10", "10 a 20", "20 a 50", ">50")))

# tabela que mostra quantos indivíduos moram em domicílios dentro da faixa de renda, filtrando apenas indivíduos com 16 anos ou mais
resumo <- pnadc %>% 
    filter(V2009>=16) %>% 
    group_by(faixa_dmcl) %>% 
    summarise(x=survey_prop())

# imprime o resultado final
print(resumo)

