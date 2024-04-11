#SE NECESITAN LOS PAQUETES 
#install.packages("stringi")
#**************************
rm(list=ls())#Limpiar espacio de trabajo
graphics.off()#Borrar graficos de sesiones anteriores


setwd("D:/SebaGIt/SentimientoVF/IPoM/pdf")
listief <- dir(pattern = "*.pdf")
path_excel="D:/SebaGIt/SentimientoVF/IPoM/Excel/"
paginas <- read_excel(paste0(path_excel,"paginas.xlsx") ) 
path_plano="D:/SebaGIt/SentimientoVF/IPoM/Cuerpo/ipom_plano/"
path_x_palabra="D:/SebaGIt/SentimientoVF/IPoM/Cuerpo/ipom_x_palabra/"

source("D:/SebaGIt/librerias/librerias.R")
source("D:/SebaGIt/SentimientoVF/IPoM/limpieza_ief.R")

for (i in (1:72)){
  inicioRec=as.numeric(paginas[i,5]) #Inicio recuadro
  finRec=as.numeric(paginas[i,6])
  inicioD=as.numeric(paginas[i,7]) #Decisiones de los ?ltimos 3 meses - ESTO NO VA EN CUERPO
  inicioCap=as.numeric(paginas[i,9]) #Capitulos
  finCap=as.numeric(paginas[i,10]) 
  #finAnexos=as.numeric(paginas[i,12]) #Fin con anexos
  #NOTA: Para procesar incluyendo anexos se propone realizar variable que obtenga el max entre finCap y finAnexos
  
  #Si tiene recuadro (recuadro is not NA)
  if (!is.na(inicioRec)) { 
    
    #Si tiene decisiones (decisiones is not NA)
    if (!is.na(inicioD)) { 
      list_output1 <- pdftools::pdf_text(listief[i])[inicioRec:finRec] #Unimos recuadro
      list_output2 <- pdftools::pdf_text(listief[i])[inicioCap:finCap] #Con cap?tulos
      list_output3<-c(list_output1,list_output2) #Unimos vectores txt
      mifuncion(output=list_output3 , k=i)
    } else { #No tiene decisiones
      list_output <- pdftools::pdf_text(listief[i])[inicioRec:finCap]
      mifuncion(output=list_output , k=i)
    }
    
  } else { #no tiene recuadro, entonces comienza desde el inicio de cap?tulos
    list_output <- pdftools::pdf_text(listief[i])[inicioCap:finCap]
    mifuncion(output=list_output , k=i)
  }
  
}
