
setwd("U:\\Desktop\\SITIC-F\\")


zz <- file("aggregazione.out", open="wt")
sink(zz, type="output")

library(tframe)
library(stringr)
options(width=132)

source("funzioni_sitic.r")
source("output_ts.r")

# carica tabella di riepilogo
riepilogo <- read.table("riepilogo.txt",
                        colClasses= c(rep('character',8),rep('numeric',6),rep('character',1),rep('numeric',1),
                                      rep('character',1)), dec='.',sep=';',header=TRUE)

# seleziona identificativi serie mensili ottenute indirettamente
identificativi <- riepilogo[(riepilogo$APPROCCIO=='IND' & (riepilogo$AGGIUSTAMENTO==2 | riepilogo$AGGIUSTAMENTO==3) & riepilogo$FREQUENZA==12),
                            c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","ANNO_BASE","FREQUENZA")]


if(nrow(identificativi)!=0) {
   indici  <- read.table("indicimens.txt",dec='.',sep=';',header=TRUE,
                         colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),rep('character',1)))

   indicisa  <- read.table("indicisamens.txt",dec='.',sep=';',header=TRUE,
                           colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),rep('character',1)))

   nomi <- NULL
   grezzi <- NULL
   sa <- NULL

   for (ii in 1:nrow(identificativi)){
      tempg <- seriesitic2(identificativi[ii,],indici)$serie
      temp <- seriesitic2(identificativi[ii,],indicisa)
      tempsa <- temp$serie
      tempnome <- temp$nomeserie
      nomi <- c(nomi,tempnome)
      grezzi <- tbind(grezzi,tempg)
      sa <- tbind(sa,tempsa)
   }

   nomi <- ifelse(nchar(nomi)>40,substr(nomi,1,40),nomi)

   seasc <- grezzi-sa
   if (length(nomi)>1){
      colnames(grezzi) <- nomi
      colnames(sa) <- nomi
      colnames(seasc) <- nomi
   } else {
      names(grezzi) <- nomi
      names(sa) <- nomi
      names(seasc) <- nomi
   }

   pdf("aggregazione_mens.pdf",paper='a4r',width=0)
   oldpar <- par(no.readonly = T)
   if (length(nomi)>1){
      for (ii in nomi){
         par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
         ts.plot(tbind(grezzi[,ii],sa[,ii],pad.start=FALSE),col=c(1,2),gpars=list(las=1),lty=c(2,1),lwd=c(1,2),xlab='',ylab='',main='serie grezza e destagionalizzata')
         legend("bottomleft",legend=c('serie grezza','serie destagionalizzata'),col=c(1,2),lty=c(2,1),lwd=c(1,2),inset=0.01)

         ts.plot(trimNA(seasc[,ii]),col=c(1),gpars=list(las=1),lty=c(1),lwd=c(1),xlab='',ylab='',main='componente stagionale')

         spectrum(tbind(grezzi[,ii],sa[,ii],pad.start=FALSE),col=c(1,2),spans=c(3,3),main='periodogramma')
         legend(x='topright',legend=c('serie grezza','serie destagionalizzata'),lty=c(1,1),col=c(1,2),inset=0.01)
         mtext(line=0,ii,outer=T,font=2,cex=1.2)
      }
   } else {
      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      ts.plot(tbind(grezzi,sa,pad.start=FALSE),col=c(1,2),gpars=list(las=1),lty=c(2,1),lwd=c(1,2),xlab='',ylab='',main='serie grezza e destagionalizzata')
      legend("bottomleft",legend=c('serie grezza','serie destagionalizzata'),col=c(1,2),lty=c(2,1),lwd=c(1,2),inset=0.01)

      ts.plot(trimNA(seasc),col=c(1),gpars=list(las=1),lty=c(1),lwd=c(1),xlab='',ylab='',main='componente stagionale')

      spectrum(tbind(grezzi,sa,pad.start=FALSE),col=c(1,2),spans=c(3,3),main='periodogramma')
      legend(x='topright',legend=c('serie grezza','serie destagionalizzata'),lty=c(1,1),col=c(1,2),inset=0.01)
      mtext(line=0,ii,outer=T,font=2,cex=1.2)
   }
   par <- oldpar
   dev.off()

   # usa tsplus per verificare stagionalita residua

   # scrivi file per TS con modelli orig
   con <- textConnection('tsinput',open='w')
   for (ii in nomi){
      if ( length(nomi)==1){
         temp <- sa
      } else {
         temp <- sa[,ii]
      }
      cat(file=con,ii,"\n")
      cat(file=con,length(trimNA(temp)),start(trimNA(temp)),frequency(temp),"\n")
      cat(file=con,trimNA(temp),sep="\n")
      cat(file=con,'$INPUT RSA=3 HPCYCLE=0 OUT=0 $END',sep="\n")
   }
   close(con)
   firstval <- grep("INPUT",tsinput)[1]
   tsinput[firstval] <- sub("INPUT ","INPUT ITER=3 ",tsinput[firstval])
   write(tsinput,"serietsplusm.txt")

   # elimina cartelle preesistenti
   unlink('outtsplusm',recursive=TRUE)
   unlink('outgtsplusm',recursive=TRUE)

   if(Sys.info()["sysname"]!='Linux'){
      # esegui ts - comandi DOS
      system("C:\\TRAMO\\TRAMO.EXE  -i serietsplusm.txt -o outtsplusm -g outgtsplusm",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("C:\\SEATS\\SEATS.EXE -i seats.itr  -o outtsplusm -g outgtsplusm",ignore.stdout = TRUE,ignore.stderr = TRUE)
   } else {
      # esegui ts - comandi linux
      system("tramo_942 -i serietsplusm.txt -o outtsplusm -g outgtsplusm",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("seats_942 -i seats.itr  -o outtsplusm -g outgtsplusm",ignore.stdout = TRUE,ignore.stderr = TRUE)
   }
   unlink('outgtsplusm',recursive=TRUE)

   out <- output_tsplus('outtsplusm',freq=12)

   cat("######################################################","\n")
   cat("Test di stagionalita serie mensili destagionalizzate indirettamente","\n")
   cat("######################################################","\n")
   print(out$TsignifS[,c(1,2,4,6,8,10)])

} else { 
   cat("######################################################","\n")
   cat("Non ci sono serie mensili destagionalizzate indirettamente","\n")
   cat("######################################################","\n")
}


# TRIMESTRALI

# seleziona identificativi serie trimestrali ottenute indirettamente
identificativi <- riepilogo[(riepilogo$APPROCCIO=='IND' & (riepilogo$AGGIUSTAMENTO==2 | riepilogo$AGGIUSTAMENTO==3) & riepilogo$FREQUENZA==4),
                            c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","ANNO_BASE","FREQUENZA")]


if(nrow(identificativi)!=0) { 

   indici  <- read.table("indicitrim.txt",dec='.',sep=';',header=TRUE,
                         colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),rep('character',1)))

   indicisa  <- read.table("indicisatrim.txt",dec='.',sep=';',header=TRUE,
                           colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),rep('character',1)))


   nomi <- NULL
   grezzi <- NULL
   sa <- NULL

   for (ii in 1:nrow(identificativi)){
      tempg <- seriesitic2(identificativi[ii,],indici)$serie
      temp <- seriesitic2(identificativi[ii,],indicisa)
      tempsa <- temp$serie
      tempnome <- temp$nomeserie
      nomi <- c(nomi,tempnome)
      grezzi <- tbind(grezzi,tempg)
      sa <- tbind(sa,tempsa)
   }
   nomi <- ifelse(nchar(nomi)>40,substr(nomi,1,40),nomi)

   seasc <- grezzi-sa
   if (length(nomi)>1){
      colnames(grezzi) <- nomi
      colnames(sa) <- nomi
      colnames(seasc) <- nomi
   } else {
      names(grezzi) <- nomi
      names(sa) <- nomi
      names(seasc) <- nomi
   }


   pdf("aggregazione_trim.pdf",paper='a4r',width=0)
   oldpar <- par(no.readonly = T)
   if (length(nomi)>1){
      for (ii in nomi){
         par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
         ts.plot(tbind(grezzi[,ii],sa[,ii],pad.start=FALSE),col=c(1,2),gpars=list(las=1),lty=c(2,1),lwd=c(1,2),xlab='',ylab='',main='serie grezza e destagionalizzata')
         legend("bottomleft",legend=c('serie grezza','serie destagionalizzata'),col=c(1,2),lty=c(2,1),lwd=c(1,2),inset=0.01)

         ts.plot(trimNA(seasc[,ii]),col=c(1),gpars=list(las=1),lty=c(1),lwd=c(1),xlab='',ylab='',main='componente stagionale')

         spectrum(tbind(grezzi[,ii],sa[,ii],pad.start=FALSE),col=c(1,2),spans=c(3,3),main='periodogramma')
         legend(x='topright',legend=c('serie grezza','serie destagionalizzata'),lty=c(1,1),col=c(1,2),inset=0.01)
         mtext(line=0,ii,outer=T,font=2,cex=1.2)
      }
   } else {
      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      ts.plot(tbind(grezzi,sa,pad.start=FALSE),col=c(1,2),gpars=list(las=1),lty=c(2,1),lwd=c(1,2),xlab='',ylab='',main='serie grezza e destagionalizzata')
      legend("bottomleft",legend=c('serie grezza','serie destagionalizzata'),col=c(1,2),lty=c(2,1),lwd=c(1,2),inset=0.01)

      ts.plot(trimNA(seasc),col=c(1),gpars=list(las=1),lty=c(1),lwd=c(1),xlab='',ylab='',main='componente stagionale')

      spectrum(tbind(grezzi,sa,pad.start=FALSE),col=c(1,2),spans=c(3,3),main='periodogramma')
      legend(x='topright',legend=c('serie grezza','serie destagionalizzata'),lty=c(1,1),col=c(1,2),inset=0.01)
      mtext(line=0,ii,outer=T,font=2,cex=1.2)
   }
   par <- oldpar
   dev.off()

   # usa tsplus per verificare stagionalita residua

   # scrivi file per TS con modelli orig
   con <- textConnection('tsinput',open='w')
   for (ii in nomi){
      if ( length(nomi)==1){
         temp <- sa
      } else {
         temp <- sa[,ii]
      }
      cat(file=con,ii,"\n")
      cat(file=con,length(trimNA(temp)),start(trimNA(temp)),frequency(temp),"\n")
      cat(file=con,trimNA(temp),sep="\n")
      cat(file=con,'$INPUT RSA=3 OUT=0 HPCYCLE=0 $END',sep="\n")
   }
   close(con)
   firstval <- grep("INPUT",tsinput)[1]
   tsinput[firstval] <- sub("INPUT ","INPUT ITER=3 ",tsinput[firstval])
   write(tsinput,"serietsplust.txt")

   # elimina cartelle preesistenti
   unlink('outtsplust',recursive=TRUE)
   unlink('outgtsplust',recursive=TRUE)

   if(Sys.info()["sysname"]!='Linux'){
      # esegui ts - comandi DOS
      system("C:\\TRAMO\\TRAMO.EXE  -i serietsplust.txt -o outtsplust -g outgtsplust",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("C:\\SEATS\\SEATS.EXE -i seats.itr  -o outtsplust -g outgtsplust",ignore.stdout = TRUE,ignore.stderr = TRUE)
   } else {
      # esegui ts - comandi linux
      system("tramo_942 -i serietsplust.txt -o outtsplust -g outgtsplust",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("seats_942 -i seats.itr  -o outtsplust -g outgtsplust",ignore.stdout = TRUE,ignore.stderr = TRUE)
   }
   unlink('outgtsplust',recursive=TRUE)

   out <- output_tsplus('outtsplust',freq=4)

   cat("######################################################","\n")
   cat("Test di stagionalita serie trimestrali destagionalizzate indirettamente","\n")
   cat("######################################################","\n")
   print(out$TsignifS[,c(1,2,4,6,8,10)])

} else { 
   cat("######################################################","\n")
   cat("Non ci sono serie trimestrali destagionalizzate indirettamente","\n")
   cat("######################################################","\n")
}
