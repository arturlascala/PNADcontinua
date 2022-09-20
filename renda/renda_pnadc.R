# DISCLAIMER
# Este é uma análise exploratória, criada para ajudar a sanar dúvidas que eu tenho a respeito da PNAD Contínua e sua relação com a amostragem em pesquisas eleitorais. 
# Eu não sou especialista nas bases do IBGE nem em pesquisas eleitorais. Qualquer análise decorrente deste script deve ser lida com ceticismo. 


library(PNADcIBGE)
library(tidyverse)

### dados preliminares
ano <- 2021 # Preencha o ano
trimestre <- 4 # Preencha o trimestre
minimo <- 1100 # Preencha o valor do salário mínimo para o ano em questão (https://www.contabeis.com.br/tabelas/salario-minimo/)

### carrega dados da PNAD da API do IBGE
pnadc <- get_pnadc(year = ano, quarter = trimestre, design = FALSE)

### agrega dados por domicílio, excluindo menores de 16 anos da contagem de indivíduos
pnadc_agg <- pnadc %>% 
    filter(V2009>=16) %>% 
    group_by(UPA, V1008) %>% 
    summarise(renda=sum(VD4019, na.rm = T), pessoas_maiores_domicilio=n()) 

### agrupa conforme faixas de renda
faixas_de_renda <- pnadc_agg %>% 
    mutate(faixa=cut(renda, 
                     breaks=c(-Inf, minimo*2, minimo*3, minimo*5, minimo*10, minimo*20, minimo*50, Inf),
                     labels=c("0 a 2", "2 a 3", "3 a 5", "5 a 10", "10 a 20", "20 a 50", ">50"))) %>% 
    select(UPA, V1008, renda, faixa) 

### tabela resumo: indivíduos maiores de 16 anos agrupados por faixa de renda do domicílio
renda_domiciliar <- faixas_de_renda %>% 
    group_by(faixa) %>% 
    summarise(qtd=n()) %>% mutate(faixa_pct=qtd/sum(qtd))

### tabela que eu SUPONHO que seja utilizada pela Quaest para estimar a proporção de indivíduos na faixa de 0 a 2 salários
faixas_de_renda_quaest <- pnadc %>% 
    filter(V2009>=16) %>% 
    mutate(faixa=cut(VD4019, 
                     breaks=c(-Inf, minimo*2, minimo*3, minimo*5, minimo*10, minimo*20, minimo*50, Inf),
                     labels=c("0 a 2", "2 a 3", "3 a 5", "5 a 10", "10 a 20", "20 a 50", ">50"))) %>% 
    select(VD4019, faixa) %>% 
    group_by(faixa) %>% 
    summarise(qtd=n()) %>% mutate(faixa_pct=qtd/sum(qtd))

