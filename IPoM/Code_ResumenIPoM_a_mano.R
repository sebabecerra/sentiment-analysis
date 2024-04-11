#SE NECESITAN LOS PAQUETES 
#install.packages("stringi")
#**************************
rm(list=ls())#Limpiar espacio de trabajo
graphics.off()#Borrar graficos de sesiones anteriores
#*
#*###################################################################################################

#### ESTE ES EL CÓDIGO QUE REALIZÓ LOS RESUMENES "A MANO". Fue útil para probar que los for funcionen correctamente.

###################################################################################################

#*

library(stringi)
library(tidytext)
library(tidyverse)
library(igraph)
library(ggraph)
library(tm) # espec?fico para miner?a de textos. 
library(SnowballC)  
library(wordcloud) #para graficar nubes de palabras  
library(ggplot2) #una gram?tica de gr?ficas que expande las funciones base de R. 
library(dplyr) # con funciones auxiliares para manipular y transformar datos. En particular, el operador %>% permite escribir funciones m?s legibles para seres humanos.
library(readr) # facilitar? leer y escribir documentos. 
library(cluster) # con funciones para realizar an?lisis de grupos. 
library(NLP) 
library(RColorBrewer) 
library(pdftools)
library(wordcloud2)
library(xlsx)
library(textclean)
library(githubinstall)
library(pluralize)
library(stringr)
library(network)
library(readxl)

#Loop que extre IEF desde el PDF al txt
mifuncion <- function(output, k) {
  
  ief <- gsub("[[:cntrl:]]", " ", output)
  ief <- gsub('(\\s|\r|\n)+',' ',paste(unlist(ief)))
  ief <- tolower(ief)           #convirtiendo todo a minusculas.
  #ief <- replace_contraction(ief) # reemplaza la contracci?n por las 2 palabras
  ief <- removePunctuation(ief) #se  deshace de la puntuaci?n.
  ief <- removeNumbers(ief)     #En este caso, removemos los n?meros
  
  #exceptions <- c("más")
  #my_stopwords1 <- setdiff(stopwords("spanish"), exceptions)
  #ief <- removeWords(ief, my_stopwords1)
  
  
  ief <- removeWords(ief, words = stopwords("spanish"))
  
  #PONERAQUI O MAS ADELANTE
  
  ief <- str_replace_all(ief,"[[:punct:]]", " ")
  ief <- str_replace_all(ief,"[[:digit:]]", " ")
  ief <- str_replace_all(ief,"[\\s]+", " ")
  ief <- str_replace_all(ief,"[^[:graph:]]", " ")
  ief <- stri_trans_general(ief,"Latin-ASCII")
  ief <- gsub("'s", " ", ief) #ojo si quitar
  ief <- iconv(ief, "latin1", "ASCII",sub="")
  
  
  #stopwords en inglés excepto negaciones
  exceptions <- c("no", "not", "nobody", "none", "never", "neither", "cannot")
  my_stopwords <- setdiff(stopwords("en"), exceptions)
  ief <- removeWords(ief, my_stopwords)
  
  ief <- removeWords(ief, words = "chile")
  ief <- removeWords(ief, words = c("a",	"al",	"algo",	"algunas",	"algunos",	"ante",	"antes",	"como",	"con",
                                    "contra",	"cual",	"cuando",	"de",	"del",	"desde",	"donde",	"durante",	"e",
                                    "el",	"el",	"ella",	"ellas",	"ellos",	"en",	"entre",	"era",	"erais",	"eramos",
                                    "eran",	"eras",	"eres",	"es",	"esa",	"esas",	"ese",	"eso",	"esos",	"esta",	"esta",
                                    "estaba",	"estabais",	"estabamos",	"estaban",	"estabas",	"estad",	"estada",	"estadas",
                                    "estado",	"estados",	"estais",	"estamos",	"estan",	"estando",	"estar",	"estara",	"estaran",
                                    "estaras",	"estare",	"estareis",	"estaremos",	"estaria",	"estariais",	"estariamos",
                                    "estarian",	"estarias",	"estas",	"estas",	"este",	"este",	"esteis",	"estemos",
                                    "esten",	"estes",	"esto",	"estos",	"estoy",	"estuve",	"estuviera",	"estuvierais",
                                    "estuvieramos",	"estuvieran",	"estuvieras",	"estuvieron",	"estuviese",	"estuvieseis",
                                    "estuviesemos",	"estuviesen",	"estuvieses",	"estuvimos",	"estuviste",	"estuvisteis",
                                    "estuvo",	"fue",	"fuera",	"fuerais",	"fueramos",	"fueran",	"fueras",	"fueron",
                                    "fuese",	"fueseis",	"fuesemos",	"fuesen",	"fueses",	"fui",	"fuimos",	"fuiste",	
                                    "fuisteis",	"ha",	"habeis",	"habia",	"habiais",	"habiamos",	"habian",	"habias",	
                                    "habida",	"habidas",	"habido",	"habidos",	"habiendo",	"habra",	"habran",	"habras",	
                                    "habre",	"habreis",	"habremos",	"habria",	"habriais",	"habriamos",	"habrian",
                                    "habrias",	"han",	"has",	"hasta",	"hay",	"haya",	"hayais",	"hayamos",	"hayan",
                                    "hayas",	"he",	"hemos",	"hube",	"hubiera",	"hubierais",	"hubieramos",	"hubieran",
                                    "hubieras",	"hubieron",	"hubiese",	"hubieseis",	"hubiesemos",	"hubiesen",	"hubieses",
                                    "hubimos",	"hubiste",	"hubisteis",	"hubo",	"la",	"las",	"le",	"les",	"lo",	"los",
                                    "me",	"mi",	"mi",	"mia",	"mias",	"mio",	"mios",	"mis",	"mucho",	"muchos",
                                    "muy",	"nada",	"ni",	"no",	"nos",	"nosotras",	"nosotros",	"nuestra",	"nuestras",
                                    "nuestro",	"nuestros",	"o",	"os",	"otra",	"otras",	"otro",	"otros",	"para",	"pero",
                                    "poco",	"por",	"porque",	"que",	"que",	"quien",	"quienes",	"se",	"sea",	"seais",
                                    "seamos",	"sean",	"seas",	"sera",	"seran",	"seras",	"sere",	"sereis",	"seremos",
                                    "seria",	"seriais",	"seriamos",	"serian",	"serias",	"si",	"sido",	"siendo",	"sin",
                                    "sobre",	"sois",	"somos",	"son",	"soy",	"su",	"sus",	"suya",	"suyas",	"suyo",
                                    "suyos",	"tambien",	"tanto",	"te",	"tendra",	"tendran",	"tendras",	"tendre",
                                    "tendreis",	"tendremos",	"tendria",	"tendriais",	"tendriamos",	"tendrian",
                                    "tendrias",	"tened",	"teneis",	"tenemos",	"tenga",	"tengais",	"tengamos",	
                                    "tengan",	"tengas",	"tengo",	"tenia",	"teniais",	"teniamos",	"tenian",	"tenias",
                                    "tenida",	"tenidas",	"tenido",	"tenidos",	"teniendo",	"ti",	"tiene",	"tienen",
                                    "tienes",	"todo",	"todos",	"tu",	"tu",	"tus",	"tuve",	"tuviera",	"tuvierais",
                                    "tuvieramos",	"tuvieran",	"tuvieras",	"tuvieron",	"tuviese",	"tuvieseis",
                                    "tuviesemos",	"tuviesen",	"tuvieses",	"tuvimos",	"tuviste",	"tuvisteis",
                                    "tuvo",	"tuya",	"tuyas",	"tuyo",	"tuyos",	"un",	"una",	"uno",	"unos",
                                    "vosotras",	"vosotros",	"vuestra",	"vuestras",	"vuestro",	"vuestros",	"y",
                                    "ya",	"yo"))
  ief <- stripWhitespace(ief) 
  ief <- removeWords(ief, words = c("informe estabilidad financiera", "banco central chile",
                                    "bcch", "sbif", "cmf", "svs", "s s s"))
  ief <- removeWords(ief, words = c("superintendencia bancos instituciones financieras",
                                    "parte", "dias", "dia", "puede",
                                    "veces", "chilena"))
  ief <- removeWords(ief, words = c("comision el mercado financiero", "sin embargo", "bloomberg",
                                    "ultimos"))
  ief <- removeWords(ief, words = c("superintendencia valores seguros", "si bien", "ano",
                                    "anos", "v", "vi", "mil", "ser", "uf", "valor", "aunque", "incluye"))
  ief <- removeWords(ief, words = c("elabaracion en base a informacion propia", "informacion",
                                    "elaboracion propia"))
  ief <- removeWords(ief, words = c("miles", "millones", "pesos", "dolares", "debido", "meses","partir"))
  ief <- removeWords(ief, words = c("ene" , "feb" , "mar", "abr", "may", "jun", "jul", "ago", "sep",
                                    "oct", "nov", "dic", "año", "trimestre", "primer", "ultimo",
                                    "eeuu", "japon", "china" ))
  ief <- removeWords(ief, words = c("enero" , "febrero" , "marzo", "abril", "mayo", "junio", "julio",
                                    "agosto", "septiembre", "octubre", "noviembre", "diciembre" ))
  ief <- removeWords(ief, words = c("jan" , "feb" , "mar", "apr", "may", "jun", "jul", "aug",
                                    "sep", "oct", "nov", "dec", "year", "quarterly" ))
  ief <- removeWords(ief, words = c("figure", "source", "will", "also", "can","i", "ii", "iii",
                                    "iv", "V","sample","among","based", "system"))
  ief <- removeWords(ief, words = c("-", "-","--","--","--","-","|", "pb" , "ano", "hacia", "local", "aun"))
  ief <- removeWords(ief, words = c("percent", "percentage", "percentile", "ademas", "valores"))
  ief <- removeWords(ief, words = c("real", "last","half","first","share","new","one","level",
                                    "year","fsr","due","type","use","years","since"))
  ief <- removeWords(ief, words = c("givem","fmc","lower","box","chapter","number","anual",
                                    "low","two","however","shocks","table","global","data", "sistemas"))
  ief <- removeWords(ief, words = c("central","used","thus","<U+FFFD>", "us", "informe", "traves",
                                    "asi", "segun"))
  ief <- removeWords(ief, words = c("segundo","tercero","anterior","total", "tercer", "caso", "dos",
                                    "fines", "ver", "numero", "recuadro"))
  ief <- removeWords(ief, words = c("cabe destacar", "euro", "zona", "siglas", "ingles", "puntos",
                                    "porcentuales", "reino unido", "america latina", "x", "series",
                                    "detalle", "percentil", "mediana", "x", "primer semestre",
                                    "segundo semestre"))
  ief <- removeWords(ief, words = c("de", "cabe senalar", "cierre", "estadistico", "anexo", "per",
                                    "capita", "financial", "stability", "igual", "mismo", "periodo",
                                    "representan cerca", "desviacion estandar", "ultima decada"))
  
  #espa?ol
  ief <- removeWords(ief, words = c("banco banco", "bancos grandes", "bancos medianos",
                                    "bancos tesoreria","bancos retail", "banco retail", "nd",
                                    "ecc","decil", "estrato", "ncg", "n", "a","aa","aaa",
                                    "aaaa","b", "bb", "bbb", "bbbb", "c", "ccc", "ion","c", "s",
                                    "graficos", "grafico","base","the","nivel","fuente","tipo",
                                    "información","plazo","ief","semestre","porcentaje","promedio",
                                    "torno", "considera", "principalmente", "observado", "mientras",
                                    "muestran", "puden", "podria"))
  ief <- removeWords(ief, words = c("alza", "alzas", "brasil","colombia", "bis", "bcbs", "q", "n","dan", "linea",
                                    "vertical", "horizontal", "punteada", "capitulo","tabla", "tablas",
                                    "paises","tasas","and","cada","índice","indice","años","mayor","for","'",
                                    "encuesta financiera de hogares", "efh", "banco inglaterra", "mas", "cuyo",
                                    "estadisticas", "estadisticos", "estadistica", "estadistico", "estadisticamente", "estadisticaseconomicasfinancierahogar"
                                    , "estadisticaseconomicasfinancierahogar", "estadisitico", "estadisticaseconomicasfinancierahogar"))
  ief <- gsub("c redito", "credito", ief)
  ief <- gsub("creditos", "credito", ief)
  ief <- #gsub("credito", "creditos", ief)
    ief <- gsub("ries go", "riesgo", ief)
  ief <- gsub("riesgos", "riesgo", ief)
  ief <- #gsub("riesgo", "riesgos", ief)
    ief <- gsub("hogares", "hogar", ief)
  ief <- #gsub("hogar", "hogares", ief)
    ief <- gsub("bancos", "banco", ief)
  ief <- gsub("banca", "banco", ief)
  ief <- gsub("bancorios", "bancorio", ief)
  ief <- gsub("bancorias", "bancoria", ief)
  ief <- gsub("bancorio", "bancario", ief)
  ief <- gsub("bancoria", "bancaria", ief)
  #ief <- gsub("banco centrales", "bancos centrales", ief)
  #ief <- gsub("banco", "bancos", ief)
  #ief <- gsub("bancorias", "bancaria", ief)
  #ief <- gsub("bancorios", "bancario", ief)
  #ief <- gsub("bancorio", "bancario", ief)
  #ief <- gsub("bancosrio", "bancaria", ief)
  #ief <- gsub("bancoria", "bancaria", ief)
  #ief <- gsub("bancosria", "bancarias", ief)
  ief <- gsub("empresas", "empresa", ief)
  ief <- gsub("empresa", "empresas", ief)
  ief <- gsub("firmas", "firma", ief)
  ief <- gsub("firma", "empresas", ief)
  ief <- gsub("credito bancaria", "credito bancario", ief)
  ief <- gsub("sistema bancaria", "sistema bancario", ief)
  ief <- gsub("'", " ", ief)
  ief <- gsub("-", " ", ief)
  ief <- gsub("-", " ", ief)
  ief <- gsub("  ", " ", ief)
  ief <- gsub(" - ", " ", ief)
  ief <- gsub(" - ", " ", ief)
  ief <- stripWhitespace(ief)   #Por ?ltimo eliminamos los espacios vacios excesivos, muchos de ellos introducidos por las transformaciones anteriores.
  #ief <- gsub("superintendencia bancos instituciones financieras", "", ief)
  ief <- stripWhitespace(ief)
  ief2 <- paste(unlist(ief),collapse=" ")
  
  #aqui ief plano
  ief2 <- ief2[ief2!=""]
  ief2 <- ief2[ief2!=" "]
  ief2 <- stripWhitespace(ief2)
  #path1= "D:/Trabajo/TextMining/ProyectoFinal/Cuerpo/ief_plano/" 
  
  path1= "C:/Users/Asus/Desktop/Practica/resumen/resumenIPoM_plano"
  
  write.csv(ief2, paste0(path1, gsub("IEF","", gsub(".pdf","",listief[k])),".txt"))
  
  #termina linea plano
  
  ief3 <- strsplit(ief2, split =  " ") %>% unlist
  ief3 <- gsub("[[:cntrl:]]", " ", ief3)
  ief3 <- gsub('(\\s|\r|\n)+',' ',paste(unlist(ief3)))
  ief3 <- stripWhitespace(ief3)
  #ief3 <- sapply(ief3, replace_contraction)
  #ief3 <- singularize(ief3)
  ief3 <- gsub("'s", " ", ief3)
  ief3 <- gsub("banco banco", " ", ief3)
  ief3 <- gsub("empresas empresas", " ", ief3)
  ief3 <- gsub("www\\S+\\s*","", ief3)
  ief3 <- gsub("http\\S+\\s*","", ief3)
  ief3 <- gsub("ulneravilidades","vulneravilidades", ief3)
  ief3 <- ief3[ief3!=""]
  ief3 <- ief3[ief3!=" "]
  
  #path= "D:/Trabajo/TextMining/ProyectoFinal/Cuerpo/ief_x_palabra/"
  path= "C:/Users/Asus/Desktop/Practica/resumen/resumenIPoM_x_palabra"
  #write.csv(ief3, file=paste("ief_x_palabra",k,".txt")) 
  #write.csv(dt,paste(path_out,'my_file.csv',sep = ''))
  write.csv(ief3, paste0(path, gsub("IEF","", gsub(".pdf","",listief[k])),".txt"))
  #???write.csv(ief3, file=paste(k,".txt"))
}

### Cortar PDF solo capitulos y recuadros ###

#setwd("N:/DPF/GEF/DAPR/Investigaci?n/2019/Analisis de Sentimiento/IEF/IEF_ESPA?OL/")

###Crea lista de IEF a extrae pdf's de la carpeta
#path = "N:/DPF/GEF/DAPR/Investigaci?n/2019/Analisis de Sentimiento/IEF/IEF_ESPA?OL/"

setwd('C:/Users/Asus/Desktop/Practica/IPoM') #Aquí están los 72 IPoMs en pdf
listief <- dir(pattern = "*.pdf")

#A cada IEF se le aplica la limpieza

#setwd("D:/Trabajo/TextMining/ProyectoFinal")

### Resúmenes de IPoM

#2001_01  
list_output <- pdftools::pdf_text(listief[1])[8:12]
mifuncion(output=list_output , k=1)

#2001_05 
list_output <- pdftools::pdf_text(listief[2])[8:13]
mifuncion(output=list_output , k=2)

#2001_09 
list_output <- pdftools::pdf_text(listief[3])[8:14] 
mifuncion(output=list_output , k=3)

#2002_01 FALTAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
list_output <- pdftools::pdf_text(listief[4])[8:13] #???????????????? 
mifuncion(output=list_output , k=4)

#2002_05 
list_output <- pdftools::pdf_text(listief[5])[10:17] 
mifuncion(output=list_output , k=5)

#2002_09 sin resumen 62 sin articulos 92 con articulos
list_output <- pdftools::pdf_text(listief[6])[10:16] 
mifuncion(output=list_output , k=6)

#2003_01 sin resumen 50 sin articulos 79 con articulos
list_output <- pdftools::pdf_text(listief[7])[10:18] #no se incluyen articulos
mifuncion(output=list_output , k=7)

#2003_05 sin resumen 67 sin articulos 102 con articulos
list_output <- pdftools::pdf_text(listief[8])[10:18]
mifuncion(output=list_output , k=8)

#2003_09 sin resumen 60 sin articulos 78 con articulos
list_output <- pdftools::pdf_text(listief[9])[10:18]
mifuncion(output=list_output , k=9)

#2004_01 sin resumen 70 sin articulos 94 con articulos
list_output <- pdftools::pdf_text(listief[10])[9:17]
mifuncion(output=list_output , k=10)

#2004_05 
list_output <- pdftools::pdf_text(listief[11])[10:17]
mifuncion(output=list_output , k=11)

#2004_09 
list_output <- pdftools::pdf_text(listief[12])[8:15]
mifuncion(output=list_output , k=12) 

#2005_01 
list_output <- pdftools::pdf_text(listief[13])[8:13]
mifuncion(output=list_output , k=13) 

#2005_05 
list_output <- pdftools::pdf_text(listief[14])[8:12]
mifuncion(output=list_output , k=14)

#2005_09 
list_output <- pdftools::pdf_text(listief[15])[8:14]
mifuncion(output=list_output , k=15)

#2006_01 
list_output <- pdftools::pdf_text(listief[16])[8:12]
mifuncion(output=list_output , k=16)

#2006_05 sin resumen 65 con todo no tiene art ni cap tematico  
list_output <- pdftools::pdf_text(listief[17])[7:12]
mifuncion(output=list_output , k=17) 

#2006_09 sin resumen 64 con todo no tiene art ni cap tematico 
list_output <- pdftools::pdf_text(listief[18])[7:13]
mifuncion(output=list_output , k=18)

#2007_01 sin resumen 48 con todo no tiene art ni cap tematico 
list_output <- pdftools::pdf_text(listief[19])[7:12]
mifuncion(output=list_output , k=19)

#2007_05 sin resumen 56 con todo no tiene art ni cap tematico 
list_output <- pdftools::pdf_text(listief[20])[7:13]
mifuncion(output=list_output , k=20)

#2007_09 sin resumen 45 con todo no tiene art ni cap tematico
list_output <- pdftools::pdf_text(listief[21])[7:13]
mifuncion(output=list_output , k=21)  

#2008_01 sin resumen 49 con todo no tiene art ni cap tematico
list_output <- pdftools::pdf_text(listief[22])[7:12]
mifuncion(output=list_output , k=22)

#2008_05 sin resumen 43 con todo no tiene art ni cap tematico
list_output <- pdftools::pdf_text(listief[23])[7:12]
mifuncion(output=list_output , k=23)

#2008_09 sin resumen 54 con todo no tiene art ni cap tematico
list_output <- pdftools::pdf_text(listief[24])[7:10]
mifuncion(output=list_output , k=24) 

#2009_01 
list_output <- pdftools::pdf_text(listief[25])[7:10]
mifuncion(output=list_output , k=25)

#2009_05 
list_output <- pdftools::pdf_text(listief[26])[7:12]
mifuncion(output=list_output , k=26)

#2009_09 
list_output <- pdftools::pdf_text(listief[27])[7:10]
mifuncion(output=list_output , k=27)

#2009_12 
list_output <- pdftools::pdf_text(listief[28])[7:10]
mifuncion(output=list_output , k=28)


#2010_03 
list_output <- pdftools::pdf_text(listief[29])[7:10]
mifuncion(output=list_output , k=29)

#2010_06 
list_output <- pdftools::pdf_text(listief[30])[7:10]
mifuncion(output=list_output , k=30) 

#2010_09 
list_output <- pdftools::pdf_text(listief[31])[7:10]
mifuncion(output=list_output , k=31)

#2010_12 
list_output <- pdftools::pdf_text(listief[32])[7:10]
mifuncion(output=list_output , k=32)

#2011_03 
list_output <- pdftools::pdf_text(listief[33])[7:10]
mifuncion(output=list_output , k=33)

#2011_06 
list_output <- pdftools::pdf_text(listief[34])[7:10]
mifuncion(output=list_output , k=34)

#2011_09 
list_output <- pdftools::pdf_text(listief[35])[7:10]
mifuncion(output=list_output , k=35)

#2011_12 
list_output <- pdftools::pdf_text(listief[36])[7:10]
mifuncion(output=list_output , k=36)

#2012_03 
list_output <- pdftools::pdf_text(listief[37])[7:10]
mifuncion(output=list_output , k=37)

#2012_06 
list_output <- pdftools::pdf_text(listief[38])[7:10]
mifuncion(output=list_output , k=38)

#2012_09 
list_output <- pdftools::pdf_text(listief[39])[7:10]
mifuncion(output=list_output , k=39)

#2012_12 
list_output <- pdftools::pdf_text(listief[40])[8:11]
mifuncion(output=list_output , k=40)

#2013_03 
list_output <- pdftools::pdf_text(listief[41])[7:10]
mifuncion(output=list_output , k=41)

#2013_06 
list_output <- pdftools::pdf_text(listief[42])[7:10]
mifuncion(output=list_output , k=42)

#2013_09 
list_output <- pdftools::pdf_text(listief[43])[8:11]
mifuncion(output=list_output , k=43)

#2013_12 
list_output <- pdftools::pdf_text(listief[44])[7:10]
mifuncion(output=list_output , k=44)

#2014_03 
list_output <- pdftools::pdf_text(listief[45])[8:11]
mifuncion(output=list_output , k=45)

#2014_06 
list_output <- pdftools::pdf_text(listief[46])[8:11]
mifuncion(output=list_output , k=46)

#2014_09 
list_output <- pdftools::pdf_text(listief[47])[8:12]
mifuncion(output=list_output , k=47)

#2014_12 
list_output <- pdftools::pdf_text(listief[48])[8:11]
mifuncion(output=list_output , k=48)

#2015_03 
list_output <- pdftools::pdf_text(listief[49])[8:11]
mifuncion(output=list_output , k=49)

#2015_06 
list_output <- pdftools::pdf_text(listief[50])[8:11]
mifuncion(output=list_output , k=50)

#2015_09 
list_output <- pdftools::pdf_text(listief[51])[8:11]
mifuncion(output=list_output , k=51)

#2015_12 
list_output <- pdftools::pdf_text(listief[52])[8:11]
mifuncion(output=list_output , k=52)

#2016_03 
list_output <- pdftools::pdf_text(listief[53])[8:11]
mifuncion(output=list_output , k=53)

#2016_06 
list_output <- pdftools::pdf_text(listief[54])[8:11]
mifuncion(output=list_output , k=54)

#2016_09 
list_output <- pdftools::pdf_text(listief[55])[8:11]
mifuncion(output=list_output , k=55)

#2016_12 
list_output <- pdftools::pdf_text(listief[56])[8:11]
mifuncion(output=list_output , k=56)

#2017_03 
list_output <- pdftools::pdf_text(listief[57])[8:11]
mifuncion(output=list_output , k=57)

#2017_06 
list_output <- pdftools::pdf_text(listief[58])[8:11]
mifuncion(output=list_output , k=58)

#2017_09 
list_output <- pdftools::pdf_text(listief[59])[8:11]
mifuncion(output=list_output , k=59)

#2017_12 
list_output <- pdftools::pdf_text(listief[60])[8:11]
mifuncion(output=list_output , k=60)

#2018_03 
list_output <- pdftools::pdf_text(listief[61])[8:11]
mifuncion(output=list_output , k=61)

#2018_06 
list_output <- pdftools::pdf_text(listief[62])[8:11]
mifuncion(output=list_output , k=62)

#2018_09 
list_output <- pdftools::pdf_text(listief[63])[8:11]
mifuncion(output=list_output , k=63)

#2018_12 
list_output <- pdftools::pdf_text(listief[64])[8:11]
mifuncion(output=list_output , k=64)

#2019_03 
list_output <- pdftools::pdf_text(listief[33])[8:12]
mifuncion(output=list_output , k=65)

#2019_06 
list_output <- pdftools::pdf_text(listief[33])[8:12]
mifuncion(output=list_output , k=66)

#2019_09 
list_output <- pdftools::pdf_text(listief[33])[8:11]
mifuncion(output=list_output , k=67)

#2019_12 
list_output <- pdftools::pdf_text(listief[33])[8:13]
mifuncion(output=list_output , k=68)

#2020_03 sin resumen 83 con todo y cap tematico
list_output <- pdftools::pdf_text(listief[33])[8:13]
mifuncion(output=list_output , k=69)

#2020_06 sin resumen 83 con todo y cap tematico
list_output <- pdftools::pdf_text(listief[33])[8:14]
mifuncion(output=list_output , k=70)

#2020_09 sin resumen 83 con todo y cap tematico
list_output <- pdftools::pdf_text(listief[33])[6:9]
mifuncion(output=list_output , k=71)

#2020_12 sin resumen 83 con todo y cap tematico
list_output <- pdftools::pdf_text(listief[33])[5:9]
mifuncion(output=list_output , k=72)
