#Codigo para procesar IPoM
#Se modifica el Codigo de COni para hacerlo mas entendible y manejable
#Podemos decidir que incluir en el procesamiento del texto: 1. recuadro, 2.desiciones, 3.cuerpo y 4.anexos. 
rm(list=ls())#Limpiar espacio de trabajo
graphics.off()#Borrar graficos de sesiones anteriores
########################################################################
# Directorio donde estarán los pdf y el .xlsx con las páginas a extraer #
#######################################################################
source("D:/SebaGIt/librerias/librerias.R")
source("D:/SebaGIt/SentimientoVF/Full/Librerias/LimpiezaIEF2.R")
raiz='D:/OneDrive/SebaGIt/SentimientoVF'
path.excel=paste0(raiz,'/excel')

informe = 'IEF'

if (informe=='IPoM'){
versiones=c('Full', 'RDC','RCA', 'RC', 'Resumen')
} else {
  versiones=c('Full', 'C', 'CT', 'FullSA', 'Resumen')
}

for (version in versiones) {
  path1=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoBruto/')
  path2=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoPlano/')
  path3=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoPalabras/')
  for ( i in c(path1, path2, path3)) {
    do.call(file.remove, list(list.files(i, full.names = TRUE)))
  }
  remove(version)
}


########################################################################
# Directorio donde estar?n los pdf y el .xlsx con las p?ginas a extraer #
#######################################################################coiscomo 

path.pdf=paste0(raiz,'/pdfs/',informe)
listief <- dir(path.pdf, pattern = "*.pdf", full.names = T)

for (version in versiones) {
  path1=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoBruto/')
  path2=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoPlano/')
  path3=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoPalabras/')
  remove(version)
}

paginas <- as.data.frame(read_excel(paste0(path.excel,'/paginas_',informe,'.xlsx'),sheet = "Cuerpo y Resumen"))
paginas <- replace(paginas,is.na(paginas),0)

if (informe=='IPoM') {  
  for (version in versiones) {
    path1=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoBruto/')
    path2=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoPlano/')
    path3=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoPalabras/')
    for (i in (1:length(listief))){
      inicio.resumen=as.numeric(paginas[i,3])
      fin.resumen=as.numeric(paginas[i,4])
      inicio.recuadro=as.numeric(paginas[i,5]) #Inicio recuadro
      fin.recuadro=as.numeric(paginas[i,6])
      inicio.decisiones=as.numeric(paginas[i,7]) #Decisiones de los últimos 3 meses - ESTO NO VA EN CUERPO
      fin.desiciones=as.numeric(paginas[i,8])
      inicio.cuerpo=as.numeric(paginas[i,9]) #Capitulos
      fin.cuerpo=as.numeric(paginas[i,10])
      inicio.anexos=as.numeric(paginas[i,11])
      fin.anexos=as.numeric(paginas[i,12]) #Fin con anexos
      resumen <- pdftools::pdf_text(listief[i])[inicio.resumen:fin.resumen]
      recuadro <- pdftools::pdf_text(listief[i])[inicio.recuadro:fin.recuadro]
      desiciones <- pdftools::pdf_text(listief[i])[inicio.decisiones:fin.desiciones]
      cuerpo <- pdftools::pdf_text(listief[i])[inicio.cuerpo:fin.cuerpo]
      anexos <- pdftools::pdf_text(listief[i])[inicio.anexos:fin.anexos]
      if (version=='Full') {
        list_output<-c(recuadro,desiciones, cuerpo, anexos)
        mifuncion(output=list_output , k=i,l=version)
      }   else if (version=='RDC') { list_output<-c(recuadro, desiciones, cuerpo)
      mifuncion(output=list_output , k=i,l=version)
      }   else if (version=='RCA') { list_output<-c(recuadro, cuerpo, anexos)
      mifuncion(output=list_output , k=i,l=version)
      }   else if (version=='RC') { list_output<-c(recuadro, cuerpo)
      mifuncion(output=list_output , k=i,l=version)
      }   else if (version=='Resumen') { 
        mifuncion(output=resumen , k=i,l=version)
      }
    }
    remove(version)
  }
}  

if (informe=='IEF') {
  for (version in versiones) {
    path1=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoBruto/')
    path2=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoPlano/')
    path3=paste0(raiz,'/Informes/',informe,'/Total/',version,'/TextoPalabras/')
    for (i in (1:length(listief))){
      inicio.resumen=as.numeric(paginas[i,3]) #Inicio recuadro
      fin.resumen=as.numeric(paginas[i,4]) #Inicio recuadro
      inicio.cuerpo1=as.numeric(paginas[i,5]) #Inicio recuadro
      fin.cuerpo1=as.numeric(paginas[i,6]) #Inicio recuadro
      inicio.cuerpo2=as.numeric(paginas[i,7]) #Inicio recuadro
      fin.cuerpo2=as.numeric(paginas[i,8]) #Inicio recuadro
      inicio.cap.tematico=as.numeric(paginas[i,9]) #Inicio recuadro
      fin.cap.tematico=as.numeric(paginas[i,10]) #Inicio recuadro
      inicio.rec.tematico=as.numeric(paginas[i,11]) #Inicio recuadro
      fin.rec.tematico=as.numeric(paginas[i,12]) #Inicio recuadro
      inicio.articulos=as.numeric(paginas[i,13]) #Inicio recuadro
      fin.articulos=as.numeric(paginas[i,14]) #Inicio recuadro
      inicio.anexos=as.numeric(paginas[i,15]) #Inicio recuadro
      fin.anexos=as.numeric(paginas[i,16]) #Inicio recuadro
      resumen <- pdftools::pdf_text(listief[i])[inicio.resumen:fin.resumen]
      cuerpo1 <- pdftools::pdf_text(listief[i])[inicio.cuerpo1:fin.cuerpo1]
      cuerpo2 <- pdftools::pdf_text(listief[i])[inicio.cuerpo2:fin.cuerpo2]
      cap.tematico <- pdftools::pdf_text(listief[i])[inicio.cap.tematico:fin.cap.tematico]
      rec.tematico <- pdftools::pdf_text(listief[i])[inicio.rec.tematico:fin.rec.tematico]
      articulos <- pdftools::pdf_text(listief[i])[inicio.articulos:fin.articulos]
      anexos <- pdftools::pdf_text(listief[i])[inicio.anexos:fin.anexos]
      if (version=='Full') {
        list_output<-c(cuerpo1, cuerpo2, cap.tematico, rec.tematico, articulos, anexos)
        mifuncion(output=list_output , k=i,l=version)
      }   else if (version=='C') { list_output<-c(cuerpo1, cuerpo2)
      mifuncion(output=list_output , k=i,l=version)
      }   else if (version=='CT') { list_output<-c(cuerpo1, cuerpo2, cap.tematico, rec.tematico)
      mifuncion(output=list_output , k=i,l=version)
      }  else if (version=='FullSA') { list_output<-c(cuerpo1, cuerpo2, cap.tematico, rec.tematico, articulos)
      mifuncion(output=list_output , k=i,l=version)
      } else if (version=='Resumen') { 
        mifuncion(output=resumen , k=i,l=version)
      }
    }
    remove(version)
  }
}














