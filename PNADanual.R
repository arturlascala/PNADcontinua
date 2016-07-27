# Function for reading 'PNAD contínua' data with the faster 'readr' package.


# For variable 'dicionario', replace with RDA file. 
# For 2014 it is "dicPNAD2014.Rdata"

# For variable 'dados', replace with TXT file.
# For 2014 it is "PES2014.txt" or "DOM2014.txt"

# 'Pessoas' data is default. 
# For 'Domicílios' data, set 'pessoas' parameter to FALSE


PNADanual <- function(dicionario, dados, pessoas = TRUE){
        
        library(readr)
        load(dicionario)
        
        if(pessoas == TRUE){
                
                tamanho <- dicpes2014$tamanho2
                tamanho[3] <- 6
                start1 <- cumsum(tamanho)-tamanho+1
                end1 <- cumsum(tamanho)
                dicpes2014$start <- start1
                dicpes2014$end <- end1
                PNAD <<- read_fwf(dados, fwf_positions(dicpes2014$start, 
                                                       dicpes2014$end, as.character(dicpes2014$cod2)))
                
        }
        else{
                
                tamanho <- dicdom2014$tamanho
                tamanho[3] <- 6
                start1 <- cumsum(tamanho)-tamanho+1
                end1 <- cumsum(tamanho)
                dicdom2014$start <- start1
                dicdom2014$end <- end1
                PNAD <<- read_fwf(dados, fwf_positions(dicdom2014$start, 
                                                       dicdom2014$end, as.character(dicdom2014$cod)))
                
        }
}
