rm(list=ls())#Limpiar espacio de trabajo
graphics.off()#Borrar graficos de sesiones anteriores

# library(stringi)
# library(tidytext)
# library(tidyverse)
# library(igraph)
# library(ggraph)
# library(tm) # espec?fico para miner?a de textos. 
# library(SnowballC)  
# library(wordcloud) #para graficar nubes de palabras  
# library(ggplot2) #una gram?tica de gr?ficas que expande las funciones base de R. 
# library(dplyr) # con funciones auxiliares para manipular y transformar datos. En particular, el operador %>% permite escribir funciones m?s legibles para seres humanos.
# library(readr) # facilitar? leer y escribir documentos. 
# library(cluster) # con funciones para realizar an?lisis de grupos. 
# library(NLP) 
# library(RColorBrewer) 
# library(pdftools)
# library(wordcloud2)
# library(xlsx)
# library(textclean)
# library(githubinstall)
# library(pluralize)
# library(stringr)
# library(network)
# library(readxl)

source("D:/SebaGIt/SentimientoVF/Full/Librerias/LimpiezaIEF2.R")

########################################################################
# Directorio donde estar?n los pdf y el .xlsx con las p?ginas a extraer #
#######################################################################
informe = 'IEF'
path.pdf=paste0('D:/SebaGIt/SentimientoVF/pdfs/',informe)
path1=paste0('D:/SebaGIt/SentimientoVF/informes/',informe,'/Capitulos/TextoBruto/')
path2=paste0('D:/SebaGIt/SentimientoVF/informes/',informe,'/Capitulos/TextoPlano/')
path3=paste0('D:/SebaGIt/SentimientoVF/informes/',informe,'/Capitulos/TextoPalabras/')
path.excel=paste0('D:/SebaGIt/SentimientoVF/excel/')
#setwd('C:/Users/Asus/Desktop/Practica/IPoM')
listief <- dir(path.pdf, pattern = "*.pdf", full.names = T)

########################


#Loop que extre IEF desde el PDF al txt

#***********************************************************************************************
#*
paginas <- read_excel(paste0(path.excel, 'paginas_',informe,'.xlsx'),sheet = paste0('Capitulos',' ',informe)) #Hoja con p?gina por tema

#Funci?n para obtener los cap?tulos
for (i in (1:length(listief))){
  for (j in (1:(length(paginas)/3))) { #Son seis cap?tulos en el IPoM
    colinicio_capj=3*j #Los cap?tulos comienzan en la tercera columna de la hoja de c?lculo y van de 3 en 3
    inicio_capj=as.numeric(paginas[i,colinicio_capj]) #Esta es la p?gina donde empieza el cap?tulo j
    #Si tiene este cap?tulo (inicio_capj is not NA), lo obtenemos.
    if (!is.na(inicio_capj)) { 
      fin_capj=as.numeric(paginas[i,colinicio_capj+1])
      nombre_capitulo=as.character(paginas[i,colinicio_capj+2])
      
      list_output <- pdftools::pdf_text(listief[i])[inicio_capj:fin_capj] #Obtenemos cap?tulos
      #Dejamos nombre sin tildes:
      nombre_capitulo <- stri_trans_general(nombre_capitulo,"Latin-ASCII")
      
      mifuncion(output=list_output , k=i,l=nombre_capitulo)
      
    }#Si no tiene ese cap?tulo, sigue al siguiente
  }
  
}
