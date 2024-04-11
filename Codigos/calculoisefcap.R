rm(list=ls())
source("D:/SebaGIt/librerias/findex.R")
source("D:/SebaGIt/librerias/findexbi.R")
#informe='IPoM'
#versiones = c('Full', 'RDC', 'RCA', 'RC')
path.dic="D:/SebaGIt/SentimientoVF/Full/Archivos base/bcch2.csv"
path.guarda="D:/SebaGIt/SentimientoVF/Findex/"
words <- read.csv(path.dic, header = TRUE, sep=";")
words$palabra <- trimws(words$palabra)
informe='IEF'
path.texto.plano=paste0('D:/SebaGIt/SentimientoVF/informes/',informe,'/Capitulos/TextoPalabras')
files<- dir(path.texto.plano, pattern = "*.txt", full.names = TRUE)
    DF1 <- lapply(files, findex)
    DF2 <- lapply(files, findex.bi)
    DF <-rbind(DF1,DF2)
    DF <- do.call("rbind", DF)
    DF <- merge(DF,words,by="palabra", all.x=T)
    DF <- DF  %>% group_by(fecha) %>% mutate(total_positive=(frec*Positive), total_negative=(frec*Negative),total_negative_positive=(frec*Negative_positive),
                                         fpositive=sum(total_positive, na.rm=TRUE), fnegative=sum(total_negative, na.rm=TRUE),
                                         fnegative_positive=sum(total_negative_positive, na.rm = TRUE)) #, fpositive=sum(total_positive, na.rm=TRUE))

DF <- DF %>% mutate(findex=-1*(fnegative + 2*fnegative_positive-fpositive)/(fnegative+fpositive))

DF<- DF %>% group_by(fecha) %>% dplyr::summarise(Findex=mean(findex, na.rm=TRUE), Positive=mean(fpositive, na.rm=TRUE), 
                                             Negative=mean(fnegative, na.rm=TRUE), NegaPositive=mean(fnegative_positive, na.rm=TRUE))
DF <- DF  %>% mutate(mean = mean(Findex), sd=sd(Findex), findex_std=(Findex-mean)/sd)
DF <- DF %>% mutate(fecha2=str_sub(fecha, 8, (str_length(fecha)-4)))
DF <- DF %>% mutate(fecha3=str_sub(fecha,1, 6))



ggplot(DF, aes(findex_std, fecha2,colour=fecha2)) +
    geom_line(aes(group = fecha2))+
    geom_point()+ stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),
                               geom="errorbar", color="black", width=0.5)+ ggtitle("ISEF capítulos IEF") +
    stat_summary(fun.y=mean, geom="point",pch = 8, color="black")+ theme(legend.position = "none")





write.csv(DF, paste0(path.guarda,informe,'-','Capitulos','-Findex.csv'))







