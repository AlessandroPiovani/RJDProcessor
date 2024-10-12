setwd("U:\\Desktop\\SITIC-MAN\\")


zz <- file("serie_non_stagionali.out", open="wt")
sink(zz, type="output")
options(width=132)

library(tframe)
library(stringr)

source("funzioni_sitic_v7.r")
source("output_ts_v7.r")

# carica tabella di riepilogo
riepilogo <- read.table("riepilogo.txt",
                        colClasses= c(rep('character',8),rep('numeric',6),rep('character',1),rep('numeric',1),
                                      rep('character',1)), dec='.',sep=';',header=TRUE)

# seleziona identificativi serie non stagionali mensili
identificativi <- riepilogo[(riepilogo$APPROCCIO=='DIF' & riepilogo$AGGIUSTAMENTO>4 & riepilogo$FREQUENZA==12),
                            c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","ANNO_BASE","FREQUENZA")]


if(nrow(identificativi)!=0) {

# dati letti da SITIC  
# indici  <- read.table("indicimens.txt",
#                       colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),
#                                    rep('character',1)),dec='.',sep=';',header=TRUE)
# 
# indicisa  <- read.table("indicisamens.txt",
#                         colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),
#                                      rep('character',1)),dec='.',sep=';',header=TRUE)

# dati letti da CSV
indici <- read.table("indicigrezzi_manifatture_per_anzini.csv",header=TRUE,sep=";",dec=",")




   nomi <- NULL
   grezzi <- NULL

   for (ii in 1:nrow(identificativi)){
      temp <- seriesitic2(identificativi[ii,],indici)
      tempg <- temp$serie
      tempnome <- temp$nomeserie
      nomi <- c(nomi,tempnome)
      grezzi <- tbind(grezzi,tempg)
   }

   # trasforma in matrice la singola serie storica
   grezzi <- ts(as.matrix(grezzi),start=start(grezzi),frequency = frequency(grezzi))
   nomi <- ifelse(nchar(nomi)>40,substr(nomi,1,40),nomi)
   colnames(grezzi) <- nomi

   # usa tsplus per verificare stagionalita' residua

   # scrivi file per TS con modelli orig
   con <- textConnection('tsinput',open='w')
   for (ii in nomi){
      if ( length(nomi)==1){
         temp <- grezzi
      } else {
         temp <- grezzi[,ii]
      }
      cat(file=con,ii,"\n")
      cat(file=con,length(trimNA(temp)),start(trimNA(temp)),frequency(temp),"\n")
      cat(file=con,trimNA(temp),sep="\n")
      cat(file=con,'$INPUT RSA=3 HPCYCLE=0 OUT=0 $END',sep="\n")
   }
   close(con)
   firstval <- grep("INPUT",tsinput)[1]
   tsinput[firstval] <- sub("INPUT ","INPUT ITER=3 ",tsinput[firstval])
   write(tsinput,"serietsplusnonsm.txt")

   # elimina cartelle preesistenti
   unlink('outtsplusnonsm',recursive=TRUE)
   unlink('outgtsplusnonsm',recursive=TRUE)

   if(Sys.info()["sysname"]!='Linux'){
      # esegui ts - comandi DOS
      system("U:\\TRAMO\\Tramo.exe  -i serietsplusnonsm.txt -o outtsplusnonsm -g outgtsplusnonsm",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("U:\\SEATS\\Seats.exe -i seats.itr  -o outtsplusnonsm -g outgtsplusnonsm",ignore.stdout = TRUE,ignore.stderr = TRUE)
   } else {
      # esegui ts - comandi linux
      system("tramo_941 -i serietsplusnonsm.txt -o outtsplusnonsm -g outgtsplusnonsm",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("seats_941 -i seats.itr  -o outtsplusnonsm -g outgtsplusnonsm",ignore.stdout = TRUE,ignore.stderr = TRUE)
   }
   unlink('outgtsplusnonsm',recursive=TRUE)

   out <- output_tsplus('outtsplusnonsm',freq=12)

   cat("######################################################","\n")
   cat("Test di stagionalita' serie mensili non stagionali","\n")
   cat("######################################################","\n")
   print(out$TsignifS[,c(1,2,4,6,8,10)])

} else { 
   cat("######################################################","\n")
   cat("Non ci sono serie mensili non stagionali","\n")
   cat("######################################################","\n")
}



# TRIMESTRALI

# seleziona identificativi serie non stagionali mensili
identificativi <- riepilogo[(riepilogo$APPROCCIO=='DIF' & riepilogo$AGGIUSTAMENTO>4 & riepilogo$FREQUENZA==4),
                            c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","ANNO_BASE","FREQUENZA")]


if(nrow(identificativi)!=0) {
# Dati letti da SITIC  
# indici  <- read.table("indicitrim.txt",
#                       colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),
#                                    rep('character',1)),dec='.',sep=';',header=TRUE)
# 
# indicisa  <- read.table("indicisatrim.txt",
#                         colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),
#                                      rep('character',1)),dec='.',sep=';',header=TRUE)
# dati letti da CSV
indici <- read.table("indicigrezzi_manifatture_per_anzini.csv",header=TRUE,sep=";",dec=",")
  
   nomi <- NULL
   grezzi <- NULL

   for (ii in 1:nrow(identificativi)){
      temp <- seriesitic2(identificativi[ii,],indici)
      tempg <- temp$serie
      tempnome <- temp$nomeserie
      nomi <- c(nomi,tempnome)
      grezzi <- tbind(grezzi,tempg)
   }
   # trasforma in matrice la singola serie storica
   grezzi <- ts(as.matrix(grezzi),start=start(grezzi),frequency = frequency(grezzi))
   nomi <- ifelse(nchar(nomi)>40,substr(nomi,1,40),nomi)
   colnames(grezzi) <- nomi

   # usa tsplus per verificare stagionalita' residua

   # scrivi file per TS con modelli orig
   con <- textConnection('tsinput',open='w')
   for (ii in nomi){
      if ( length(nomi)==1){
         temp <- grezzi
      } else {
         temp <- grezzi[,ii]
      }
      cat(file=con,ii,"\n")
      cat(file=con,length(trimNA(temp)),start(trimNA(temp)),frequency(temp),"\n")
      cat(file=con,trimNA(temp),sep="\n")
      cat(file=con,'$INPUT RSA=3 HPCYCLE=0 OUT=0 $END',sep="\n")
   }
   close(con)
   firstval <- grep("INPUT",tsinput)[1]
   tsinput[firstval] <- sub("INPUT ","INPUT ITER=3 ",tsinput[firstval])
   write(tsinput,"serietsplusnonst.txt")

   # elimina cartelle preesistenti
   unlink('outtsplusnonst',recursive=TRUE)
   unlink('outgtsplusnonst',recursive=TRUE)

   if(Sys.info()["sysname"]!='Linux'){
      # esegui ts - comandi DOS
      system("U:\\TRAMO\\Tramo.exe  -i serietsplusnonst.txt -o outtsplusnonst -g outgtsplusnonst",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("U:\\SEATS\\SEATS.exe -i seats.itr  -o outtsplusnonst -g outgtsplusnonst",ignore.stdout = TRUE,ignore.stderr = TRUE)
   } else {
      # esegui ts - comandi linux
      system("tramo_941 -i serietsplusnonst.txt -o outtsplusnonst -g outgtsplusnonst",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("seats_941 -i seats.itr  -o outtsplusnonst -g outgtsplusnonst",ignore.stdout = TRUE,ignore.stderr = TRUE)
   }
   unlink('outgtsplusnonst',recursive=TRUE)

   out <- output_tsplus('outtsplusnonst',freq=4)

   cat("######################################################","\n")
   cat("Test di stagionalita' serie trimestrali non stagionali","\n")
   cat("######################################################","\n")
   print(out$TsignifS[,c(1,2,4,6,8,10)])

} else { 
   cat("######################################################","\n")
   cat("Non ci sono serie trimestrali non stagionali","\n")
   cat("######################################################","\n")
}
