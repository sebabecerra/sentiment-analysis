#SE NECESITAN LOS PAQUETES 
#install.packages("stringi")
#**************************
rm(list=ls())#Limpiar espacio de trabajo
graphics.off()#Borrar graficos de sesiones anteriores
#*
###################################################################################################

#### ESTE ES EL C?DIGO QUE USA <FOR> EN BASE AL ARCHIVO "PAGINAS.XLXS" QUE VA ADJUNTO
#Recordar actualizar directorios para resumenIPoM_plano, resumenIPoM_x_palabra, y obtenci?n de pdfs
###################################################################################################




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
  
  #exceptions <- c("m?s")
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
  
  
  #stopwords en ingl?s excepto negaciones
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
                                    "oct", "nov", "dic", "a?o", "trimestre", "primer", "ultimo",
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
                                    "representan cerca", "desviacion estandar", "ultima decada","banco banco", "bancos grandes", "bancos medianos",
                                    "bancos tesoreria","bancos retail", "banco retail", "nd",
                                    "ecc","decil", "estrato", "ncg", "n", "a","aa","aaa",
                                    "aaaa","b", "bb", "bbb", "bbbb", "c", "ccc", "ion","c", "s",
                                    "graficos", "grafico","base","the","nivel","fuente","tipo",
                                    "informaci?n","plazo","ief","semestre","porcentaje","promedio",
                                    "torno", "considera", "principalmente", "observado", "mientras",
                                    "muestran", "puden", "podria","alza", "alzas", "brasil","colombia", "bis", "bcbs", "q", "n","dan", "linea","vertical", "horizontal", "punteada", "capitulo","tabla", "tablas",
"paises","tasas","and","cada","?ndice","indice","a?os","mayor","for","'",
"encuesta financiera de hogares", "efh", "banco inglaterra", "mas", "cuyo",
"estadisticas", "estadisticos", "estadistica", "estadistico", "estadisticamente", "estadisticaseconomicasfinancierahogar", "estadisticaseconomicasfinancierahogar", "estadisitico", "estadisticaseconomicasfinancierahogar"))
  
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
  
  path1= "D:/SebaGIt/SentimientoVF/IPoM/Cuerpo/ipom_plano/"
  
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
  path= "D:/SebaGIt/SentimientoVF/IPoM/Cuerpo/ipom_x_palabra/"
  #write.csv(ief3, file=paste("ief_x_palabra",k,".txt")) 
  #write.csv(dt,paste(path_out,'my_file.csv',sep = ''))
  write.csv(ief3, paste0(path, gsub("IEF","", gsub(".pdf","",listief[k])),".txt"))
  #???write.csv(ief3, file=paste(k,".txt"))
}

### Cortar PDF solo capitulos y recuadros ###

#setwd("N:/DPF/GEF/DAPR/Investigaci?n/2019/Analisis de Sentimiento/IEF/IEF_ESPA?OL/")

###Crea lista de IEF a extrae pdf's de la carpeta
#path = "N:/DPF/GEF/DAPR/Investigaci?n/2019/Analisis de Sentimiento/IEF/IEF_ESPA?OL/"

setwd("D:/SebaGIt/SentimientoVF/IPoM/pdf") #Aqu? est?n los 72 IPoMs en pdf
listief <- dir(pattern = "*.pdf")

#A cada IEF se le aplica la limpieza

#setwd("D:/Trabajo/TextMining/ProyectoFinal")

### Res?menes de IPoM

#***********************************************************************************************
paginas <- read_excel("D:/SebaGIt/SentimientoVF/IPoM/Excel/paginas.xlsx")  

for (i in (1:72)){
  inicio=as.numeric(paginas[i,3])
  fin=as.numeric(paginas[i,4])
  list_output <- pdftools::pdf_text(listief[i])[inicio:fin]
  mifuncion(output=list_output , k=i)
}
#*************************************************************************************************

