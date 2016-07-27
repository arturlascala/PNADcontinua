# Function for reading 'PNAD contínua' data with the faster 'readr' package.

# For variable 'dadosPNAD', replace with TXT file. 
# FOr variable 'inputPNAD', replace with SAS file. 

PNADcontinua <- function(dadosPNAD, inputPNAD){
        
        library(SAScii)  ## if not installed, use install.packages("SAScii")
        library(readr)  ## if not installed, use install.packages("readr")
        
        # Creates data.frame with inputs
        dic <- parse.SAScii(inputPNAD)
        
        # creates references for read_fwf
        start1 <- cumsum(dic$width)-dic$width+1
        end1 <- cumsum(dic$width)
        dic$start <- start1
        dic$end <- end1
        
        # reads PNAD data
        PNADc <<- read_fwf(dadosPNAD, fwf_positions(dic$start, dic$end, dic$varname))
}

