# "mynamesmens <- NULL" seleziona tutte le serie mensili presenti, altrimenti mynamesmens <- nome_TS
# "mynamestrim <- NULL" seleziona tutte le serie trimestrali presenti, altrimenti mynamestrim <- nome_TS
# "datapartenza <- c(YYYY,PP) stabilisce la data di partenza validita dei nuovi modelli
# "outputdata <- TRUE" salva i file delle serie grezze, sa, wda in formato csv, altrimenti FALSE; default: FALSE
# "diffmod <- TRUE" salva solo i modelli variati rispetto agli originali, altrimenti FALSE per salvare tutti i
#                 modelli nuovi; default = TRUE

setwd("U:\\Desktop\\SITIC-VEO\\")

#mynamestrim <- NULL

#selezione di quelle, tra le 97, che si possono toccare. Le escluse sono passate all'indiretto

mynamestrim <- c("VELA_1_MORE_B","VELA_MORE_B","VELA_1_MORE_C","VELA_MORE_C","VELA_1_MORE_D","VELA_MORE_D","VELA_1_MORE_E","VELA_MORE_E","VELA_1_MORE_F",
"VELA_MORE_F","VELA_1_MORE_G","VELA_MORE_G","VELA_1_MORE_H","VELA_MORE_H","VELA_1_MORE_I","VELA_MORE_I","VELA_1_MORE_J","VELA_MORE_J","VELA_1_MORE_K","VELA_MORE_K",
"VELA_1_MORE_L","VELA_MORE_L","VELA_1_MORE_M","VELA_MORE_M","VELA_1_MORE_N","VELA_MORE_N","VELA_1_MORE_P","VELA_MORE_P","VELA_1_MORE_Q","VELA_MORE_Q","VELA_1_MORE_R","VELA_MORE_R","VELA_MORE_S","VELA_MORE_PS","VELA_1_ORE_B","VELA_ORE_B","VELA_1_ORE_C","VELA_ORE_C","VELA_1_ORE_D","VELA_ORE_D","VELA_1_ORE_E","VELA_ORE_E","VELA_1_ORE_F","VELA_ORE_F","VELA_1_ORE_G","VELA_ORE_G","VELA_1_ORE_H","VELA_ORE_H","VELA_1_ORE_I","VELA_ORE_I","VELA_1_ORE_J","VELA_ORE_J","VELA_1_ORE_K","VELA_ORE_K","VELA_1_ORE_L","VELA_ORE_L","VELA_1_ORE_M","VELA_ORE_M","VELA_1_ORE_N","VELA_ORE_N","VELA_1_ORE_P","VELA_ORE_P","VELA_1_ORE_Q","VELA_ORE_Q","VELA_1_ORE_S","VELA_ORE_S","VELA_1_ORE_ISS","VELA_ORE_ISS","VELA_1_ORE_LMN",
"VELA_ORE_LMN","VELA_1_ORE_GS","VELA_ORE_GS","VELA_1_ORE_PS","VELA_ORE_PS","VELA_1_ORE_IND","VELA_ORE_IND","VELA_1_ORE_SER","VELA_ORE_SER","VELA_1_ORE_NS",
"VELA_ORE_NS","VELA_1_ORE_TOT","VELA_ORE_TOT")

# serie mie (Alessandro)
# mynamestrim <- c("VELA_ORE_I", "VELA_ORE_L", "VELA_1_ORE_E", "VELA_ORE_M", "VELA_1_ORE_P", "VELA_1_ORE_G", "VELA_ORE_H")


datapartenza <- c(2023,1)
outputdata <- TRUE 
diffmod <- TRUE 

###############################################################################
zz <- file("elaborazione.out", open="wt")
sink(zz, type="output")
###############################################################################

library(tframe)
library(stringr)
options(width=96)

source("funzioni_sitic.r")
source("output_ts.r")

if (file.exists('modellitrim_nuovi.txt')){
   #### ELABORAZIONE TRIMESTRALI

   frequenza <- 4

   # se si caricano gli indici da file CSV
   #dati <- read.table("indici.csv",header=TRUE,sep=";",dec=",")
   #dati <- ts(dati[,-1],freq=4,start=c(2003,1))

   # se invece si caricano gli indici da tabella SITIC
   indici  <- read.table("indicitrim.txt",
                         colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),
                                      rep('character',1)),rep('numeric',1),dec='.',sep=';',header=TRUE)

   # carica tabella di riepilogo
   riepilogo <- read.table("riepilogo.txt",
                           colClasses= c(rep('character',8),rep('numeric',6),rep('character',1),rep('numeric',1),
                                         rep('character',1)), dec='.',sep=';',header=TRUE)
   # carica modelli originali
   modelli_orig <- read.table("modellitrim_orig.txt",
                              colClasses= c(rep('character',2),rep('numeric',1),rep('character',7),rep('numeric',8),
                                            rep('character',1),rep('numeric',1)), dec='.',sep=';',header=TRUE)

   # carica modelli nuovi
   modelli_nuovi <- read.table("modellitrim_nuovi.txt",
                               colClasses= c(rep('character',2),rep('numeric',1),rep('character',7),rep('numeric',8),
                                             rep('character',1),rep('numeric',1)), dec='.',sep=';',header=TRUE)


   # elimina righe vuote e righe che iniziano con '#' da tabella modelli_nuovi
   modelli_nuovi <- modelli_nuovi[grep("^#",modelli_nuovi[,1],invert=T),]
   modelli_nuovi <- modelli_nuovi[grep("^$",modelli_nuovi[,1],invert=T),]

   # ordina per settore nomets e progressivo
   modelli_nuovi <- modelli_nuovi[order(modelli_nuovi$SERVIZIO,modelli_nuovi$COD_LIVELLO_1,modelli_nuovi$CODICE_1,modelli_nuovi$NOME_TS,modelli_nuovi$PROGRESSIVO),]

   # seleziona nomi ts serie da destagionalizzare
   if (is.null(mynamestrim)){ 
      mynamestrim <- union( unique(modelli_orig$NOME_TS) , unique(modelli_nuovi$NOME_TS))
   } else {}

   # esegui solo se ci sono delle serie
   if(length(mynamestrim)>0){

      # scrivi file per TS con modelli orig
      con <- textConnection('tsinput',open='w')
      for (ii in mynamestrim){
         #    print(ii)
         ###################### righe per file sitic
         temp <- seriesitic(ii,modelli_orig,indici,riepilogo,siticdate=TRUE)
         ###############################################################################################
         ###################### righe per file csv
         #      temp <- list("siticts","tspar")
         #      temp$siticts <- trimNA(dati[,ii])
         #      temp$tspar <- modelli_orig[modelli_orig$NOME_TS==ii,"PARAMETRO"]
         ###############################################################################################
         if(is.null(temp)) next
         cat(file=con,ii,"\n")
         cat(file=con,length(trimNA(temp$siticts)),start(trimNA(temp$siticts)),frequency(temp$siticts),"\n")
         cat(file=con,trimNA(temp$siticts),sep="\n")
         cat(file=con,temp$tspar,sep="\n")
      }
      close(con)
      inputlines <- grep("[Ii][Nn][Pp][Uu][Tt]",tsinput)
      tsinput[inputlines][1] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input iter=3 ",tsinput[inputlines][1])
      tsinput[inputlines][-1] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input ",tsinput[inputlines][-1])
      missingout <- grep('out',tsinput[inputlines],invert=TRUE)
      if(length(missingout)>0){
        cat("Attenzione, manca il parametro out=0 nei modelli vecchi delle serie:",mynamestrim[missingout],"\n")
        tsinput[inputlines][missingout] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input out=0 ",tsinput[inputlines][missingout])
      } else {}
      write(tsinput,"serie.txt")

      # scrivi file per TS con modelli nuovi
      con <- textConnection('tsinput',open='w')
      for (ii in mynamestrim){
         #    print(which(mynamestrim==ii))
         #     print(ii)
         ###################### righe per file sitic
         temp <- seriesitic(ii,modelli_nuovi,indici,riepilogo,siticdate=TRUE)
         ###############################################################################################
         ###################### righe per file csv
         #      temp <- list("siticts","tspar")
         #      temp$siticts <- trimNA(dati[,ii])
         #      temp$tspar <- modelli_nuovi[modelli_nuovi$NOME_TS==ii,"PARAMETRO"]
         ###############################################################################################
         if(is.null(temp)) next
         cat(file=con,ii,"\n")
         cat(file=con,length(trimNA(temp$siticts)),start(trimNA(temp$siticts)),frequency(temp$siticts),"\n")
         cat(file=con,trimNA(temp$siticts),sep="\n")
         cat(file=con,temp$tspar,sep="\n")
      }
      close(con)
      inputlines <- grep("[Ii][Nn][Pp][Uu][Tt]",tsinput)
      tsinput[inputlines][1] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input iter=3 ",tsinput[inputlines][1])
      tsinput[inputlines][-1] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input ",tsinput[inputlines][-1])
      missingout <- grep('out',tsinput[inputlines],invert=TRUE)
      if(length(missingout)>0){
        cat("Attenzione, manca il parametro out=0 nei modelli nuovi delle serie:",mynamestrim[missingout],"\n")
        tsinput[inputlines][missingout] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input out=0 ",tsinput[inputlines][missingout])
      } else {}
      write(tsinput,"serienew.txt")


      # elimina cartelle preesistenti
      unlink('oldt',recursive=TRUE)
      unlink('oldgt',recursive=TRUE)
      unlink('newt',recursive=TRUE)
      unlink('newgt',recursive=TRUE)

      if(Sys.info()["sysname"]!='Linux'){
         # esegui ts - comandi DOS
         system("U:\\TRAMO\\Tramo.exe -i serie.txt -o oldt -g oldgt",ignore.stdout = TRUE,ignore.stderr = TRUE) 
         system("U:\\SEATS\\Seats.exe -i seats.itr -o oldt -g oldgt",ignore.stdout = TRUE,ignore.stderr = TRUE)
         system("U:\\TRAMO\\Tramo.exe -i serienew.txt -o newt -g newgt",ignore.stdout = TRUE,ignore.stderr = TRUE) 
         system("U:\\SEATS\\Seats.exe -i seats.itr -o newt -g newgt",ignore.stdout = TRUE,ignore.stderr = TRUE)
      } else {
         # esegui ts - comandi linux
         system("tramo_942 -i serie.txt -o oldt -g oldgt",ignore.stdout = TRUE,ignore.stderr = TRUE) 
         system("seats_942 -i seats.itr -o oldt -g oldgt",ignore.stdout = TRUE,ignore.stderr = TRUE)
         system("tramo_942 -i serienew.txt -o newt -g newgt",ignore.stdout = TRUE,ignore.stderr = TRUE) 
         system("seats_942 -i seats.itr -o newt -g newgt",ignore.stdout = TRUE,ignore.stderr = TRUE)
      }
      # elimina cartelle grafici
      unlink('oldgt',recursive=TRUE)
      unlink('newgt',recursive=TRUE)

      # carica output trimestrale
      vecchi <- output_tsplus("oldt",freq=frequenza)
      nuovi <- output_tsplus("newt",freq=frequenza)

      cat("##################################################################","\n\n")
      cat("SERIE TRIMESTRALI:","\n\n")
      cat("##################################################################","\n\n")

      cat("\n\n","Diagnostica modelli originali:","\n")
      print(vecchi$diagmat[,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])
    
      cat("\n\n","Diagnostica modelli nuovi:","\n")
      print(nuovi$diagmat[,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])

      cat("\n\n","********************************","\n")
      cat("Problemi nei modelli vecchi")
      cat("\n","********************************","\n\n")
      if (sum(vecchi$diagmat[,"QR.prob"]<0.05)>0){
         cat("\n","Statistica di LB significativa:","\n")
         print(vecchi$diagmat[vecchi$diagmat[,"QR.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 

      if (sum(vecchi$diagmat[,"N.prob"]<0.05)>0){
         cat("\n","Statistica di JB significativa:","\n")
         print(vecchi$diagmat[vecchi$diagmat[,"N.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 

      if (sum(vecchi$diagmat[,"QSR.prob"]<0.05) >0){
         cat("\n","Statistica di autocorrelazione stagionale significativa:","\n")
         print(vecchi$diagmat[vecchi$diagmat[,"QSR.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 

      if (sum(vecchi$diagmat[,"QR2.prob"]<0.05) >0){
         cat("\n","Statistica di LB sui quadrati dei residui significativa:","\n")
         print(vecchi$diagmat[vecchi$diagmat[,"QR2.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 

      if (sum(vecchi$diagmat[,"MODC"]=='Y') >0){
         cat("\n","Modelli cambiati in SEATS:","\n")
         print(vecchi$diagmat[vecchi$diagmat[,"MODC"]=='Y',c("TITLE")])} 

      if ( sum(vecchi$diagmat[,"MODA"]=='Y') >0){
         cat("\n","Modelli approssimati:","\n")
         print(vecchi$diagmat[vecchi$diagmat[,"MODA"]=='Y',c("TITLE")])} 

      if ( sum(vecchi$diagmat[,"ACF"]!='0') >0){
         cat("\n","Errore ACF:","\n")
         print(vecchi$diagmat[vecchi$diagmat[,"ACF"]!='0',c("TITLE")])
      } 

      if ( sum(vecchi$diagmat[,"CCF"]!='0') >0){
         cat("\n","Errore CCF:","\n")
         print(vecchi$diagmat[vecchi$diagmat[,"CCF"]!='0',c("TITLE")])} 


      cat("\n\n","********************************","\n")
      cat("Problemi nei modelli nuovi")
      cat("\n","********************************","\n\n")
      if (sum(nuovi$diagmat[,"QR.prob"]<0.05)>0){
         cat("\n","Statistica di LB significativa:","\n")
         print(nuovi$diagmat[nuovi$diagmat[,"QR.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 

      if (sum(nuovi$diagmat[,"N.prob"]<0.05)>0){
         cat("\n","Statistica di JB significativa:","\n")
         print(nuovi$diagmat[nuovi$diagmat[,"N.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 

      if ( sum(nuovi$diagmat[,"QSR.prob"]<0.05) >0){
         cat("\n","Statistica di autocorrelazione stagionale significativa:","\n")
         print(nuovi$diagmat[nuovi$diagmat[,"QSR.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 

      if ( sum(nuovi$diagmat[,"QR2.prob"]<0.05) >0){
         cat("\n","Statistica di LB sui quadrati dei residui significativa:","\n")
         print(nuovi$diagmat[nuovi$diagmat[,"QR2.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 

      if ( sum(nuovi$diagmat[,"MODC"]=='Y') >0){
         cat("\n","Modelli cambiati in SEATS:","\n")
         print(nuovi$diagmat[nuovi$diagmat[,"MODC"]=='Y',c("TITLE")])} 

      if ( sum(nuovi$diagmat[,"MODA"]=='Y') >0){
         cat("\n","Modelli approssimati:","\n")
         print(nuovi$diagmat[nuovi$diagmat[,"MODA"]=='Y',c("TITLE")])} 

      if ( sum(nuovi$diagmat[,"ACF"]!='0') >0){
         cat("\n","Errore ACF:","\n")
         print(nuovi$diagmat[nuovi$diagmat[,"ACF"]!='0',c("TITLE")])
      } 

      if ( sum(nuovi$diagmat[,"CCF"]!='0') >0){
         cat("\n","Errore CCF:","\n")
         print(nuovi$diagmat[nuovi$diagmat[,"CCF"]!='0',c("TITLE")])} 



      pdf("confronto_trim_orig_nuovi_SA.pdf")
      if(length(mynamestrim)!=1){
         for (ii in mynamestrim){
            temp <- NULL
            leg <- NULL
            colore <- NULL
            if (ii %in% nuovi$nomi){
               temp  <- tbind(temp,nuovi$serie_sa[,ii])
               leg <- c(leg,'nuovi')
               colore <- c(colore,1)
            }
            if (ii %in% vecchi$nomi){
               temp  <- tbind(temp,vecchi$serie_sa[,ii])
               leg <- c(leg,'originali')
               colore <- c(colore,2)
            }
            rangeser <- range(temp ,na.rm=T)
            ts.plot(temp,col=colore,gpars=list(las=1),xlab='',ylab='',main=ii)
            legend("bottom",legend=leg,col=colore,lty=1,inset=0.01)
         }
      } else {
         for (ii in mynamestrim){
            temp <- NULL
            leg <- NULL
            colore <- NULL
            if (ii %in% nuovi$nomi){
               temp  <- tbind(temp,nuovi$serie_sa)
               leg <- c(leg,'nuovi')
               colore <- c(colore,1)
            }
            if (ii %in% vecchi$nomi){
               temp  <- tbind(temp,vecchi$serie_sa)
               leg <- c(leg,'originali')
               colore <- c(colore,2)
            }
            rangeser <- range(temp ,na.rm=T)
            ts.plot(temp,col=colore,gpars=list(las=1),xlab='',ylab='',main=ii)
            legend("bottom",legend=leg,col=colore,lty=1,inset=0.01)
         }
      }
      dev.off()

   pdf("confronto_trim_orig_nuovi_WDA.pdf")
      if(length(mynamestrim)!=1){
         for (ii in mynamestrim){
            temp <- NULL
            leg <- NULL
            colore <- NULL
            if (ii %in% nuovi$nomi){
               temp  <- tbind(temp,nuovi$serie_wd[,ii])
               leg <- c(leg,'nuovi')
               colore <- c(colore,1)
            }
            if (ii %in% vecchi$nomi){
               temp  <- tbind(temp,vecchi$serie_wd[,ii])
               leg <- c(leg,'originali')
               colore <- c(colore,2)
            }
            rangeser <- range(temp ,na.rm=T)
            ts.plot(temp,col=colore,gpars=list(las=1),xlab='',ylab='',main=ii)
            legend("bottom",legend=leg,col=colore,lty=1,inset=0.01)
         }
      } else {
         for (ii in mynamestrim){
            temp <- NULL
            leg <- NULL
            colore <- NULL
            if (ii %in% nuovi$nomi){
               temp  <- tbind(temp,nuovi$serie_wd)
               leg <- c(leg,'nuovi')
               colore <- c(colore,1)
            }
            if (ii %in% vecchi$nomi){
               temp  <- tbind(temp,vecchi$serie_wd)
               leg <- c(leg,'originali')
               colore <- c(colore,2)
            }
            rangeser <- range(temp ,na.rm=T)
            ts.plot(temp,col=colore,gpars=list(las=1),xlab='',ylab='',main=ii)
            legend("bottom",legend=leg,col=colore,lty=1,inset=0.01)
         }
      }
      dev.off()




      pdf("trim_old.pdf",paper='a4r',width=0)
      for (ii in 1:length(nuovi$nomi)){
         plot(vecchi,ii)
      }
      dev.off()

      pdf("trim_new.pdf",paper='a4r',width=0)
      for (ii in 1:length(nuovi$nomi)){
         plot(nuovi,ii)
      }
      dev.off()

      # check tabella modelli per coerenza parametro LAM 
      temp1 <- modelli_nuovi[modelli_nuovi$NOME_TS %in% nuovi$nomi & modelli_nuovi$PROGRESSIVO==10,]
      temp2 <- nuovi$tfit[,c("TITLE","Lam")]
      tempm <- merge(temp1,temp2,by.x="NOME_TS",by.y="TITLE")
      tempd <- tempm$MODELLO!=tempm$Lam

      cat("\n\n","********************************","\n")
      cat("Problemi nella tabella modelli nuovi")
      cat("\n","********************************","\n\n")
      if(sum(tempd) != 0) { cat("Problema di coincidenza tra LAM e par. MODELLO nelle serie", tempm[tempd!=0,"NOME_TS"] ,sep="\n")} else {cat('')}

      # check tabella modelli
      if(length(grep('[rR][sS][aA]',modelli_nuovi$PARAMETRO)) > 0) {cat("Problema con il parametro RSA","\n")} else {cat('')}
      if(length(grep('[lL][aA][mM]=-1',modelli_nuovi$PARAMETRO)) > 0) {cat("Problema con il parametro LAM=-1","\n")} else {cat('')}
      if(length(grep('[iI][nN][iI][cC]',modelli_nuovi$PARAMETRO)) > 0) {cat("Problema con il parametro INIC","\n")} else {cat('')}
      if(length(grep('[iI][dD][iI][fF]',modelli_nuovi$PARAMETRO)) > 0) {cat("Problema con il parametro IDIF","\n")} else {cat('')}
      if(length(grep('[iI][aA][tT][iI][pP]',modelli_nuovi$PARAMETRO)) > 0) {cat("Problema con il parametro IATIP","\n")} else {cat('')}
      if(length(grep('[iI][eE][aA][sS][tT]=-1',modelli_nuovi$PARAMETRO)) > 0) {cat("Problema con il parametro IEAST=-1","\n")} else {cat('')}

      ### VECCHIA VERSIONE
      # if (exists('modelli_finali')){
      #    if (diffmod==TRUE){
      #       modelli_finali <- rbind(modelli_finali,diffmodelli(modelli_nuovi,modelli_orig))
      #    } else {
      #       modelli_finali <- rbind(modelli_finali,modelli_nuovi)
      #    }
      #    modelli_finali <- rbind(modelli_finali,diffmodelli(modelli_nuovi,modelli_orig))
      # } else {
      #    if (diffmod==TRUE){
      #       modelli_finali <- diffmodelli(modelli_nuovi,modelli_orig)
      #    } else {
      #       modelli_finali <- modelli_nuovi
      #    }
      # }

      # nuova versione
      # scrivi file modelli
      if (diffmod==TRUE){
        modelli_finali <- diffmodelli(modelli_nuovi,modelli_orig)
      } else {
        modelli_finali <- modelli_nuovi
      }

      # esporta serie in formato csv
      if(outputdata==TRUE){
         write.csv2(x=nuovi$serie_grezza,file="grezzi_trim.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
         write.csv2(x=nuovi$serie_sa,file="sa_new_trim.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
         write.csv2(x=nuovi$serie_wd,file="wda_new_trim.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
         write.csv2(x=vecchi$serie_sa,file="sa_old_trim.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
      }
   }

}
# scrivi file modelli
# modifica la data di partenza dei modelli
# inserire qui altre eventuali modifiche 
if (!is.null(modelli_finali)){
   modelli_finali$ANNO_INI <- datapartenza[1]
   modelli_finali$MESE_INI <- datapartenza[2]
   #modelli_finali[modelli_finali$ANNO_BASE==2010,]$ANNO_BASE <- 2015
}
write.table(modelli_finali,"modelli_finali.txt",quote=F,col.names=F,row.names=F,sep=";",na="null")

prepare_JDplus_file("serienew.txt", "serienewJD.txt")
prepare_JDplus_file("serie.txt", "serieJD.txt")


closeAllConnections()
# print media annua di serie vecchie e nuove grezze, wda e sa
#for (ii in 1:length(nuovi$nomi)){
#serie <- round(aggregate(cbind(vecchi$serie_grezza[,ii],vecchi$serie_wd[,ii],vecchi$serie_sa[,ii],
                        #nuovi$serie_grezza[,ii],nuovi$serie_wd[,ii],nuovi$serie_sa[,ii]
                               #),nfreq=1,FUN=mean),1)
#colnames(serie) <- c(nuovi$nomi[ii],'wda','sa','new','new_wda','new_sa')
#print(serie,print.gap=4)
#cat('\n')
#cat('avg ',round(apply(serie,2,mean),1),sep='     ','\n\n')
#}




