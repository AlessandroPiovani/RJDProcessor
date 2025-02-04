setwd('C:\\Users\\UTENTE\\Desktop\\SITIC-TUR-paper\\')

# "mynamesmens  <- NULL" seleziona tutte le serie mensili presenti, altrimenti mynamesmens <- nome_TS
# "mynamestrim  <- NULL" seleziona tutte le seri\e trimestrali presenti, altrimenti mynamestrim <- nome_TS
# "datapartenza <- c(YYYY,PP) stabilisce la data di partenza validita dei nuovi modelli
# "outputdata   <- TRUE" salva i file delle serie grezze, sa, wda in formato csv, altrimenti FALSE; default: FALSE
# "diffmod      <- TRUE" salva solo i modelli variati rispetto agli originali, altrimenti FALSE per salvare tutti i modelli nuovi; default = TRUE

# ALL IMPORTS ------------------------------------------------------------------

library(glue)
library(tframe)
library(stringr)
options(width=96)
source("funzioni_sitic_v6.r")
source("output_ts_v6.r")
source("auto_comment_v6.r")

# CONFIGS ----------------------------------------------------------------------

path_ts      <- 'C:\\'

perl         <- TRUE
outputdata   <- TRUE
diffmod      <- TRUE

auto_comment_enable             <- TRUE
always_print_full_model_comment <- FALSE # suggerito = TRUE se si sostituiscono regressori


datapartenza <- c(2023,12)
mynamesmens <- c("VATASA", "VATASC", "VATAIA", "VATPIA" ,"VATPIC","VATPSC", "VATAIC", "VATPSA")
mynamestrim <- c()

#mynamestrim <- c("WAG_D","WAG_Q", "WAG_K" ,"WAG_S", "WAG_O", "OTH_O")

# SEASONAL ADJUSTMENT ----------------------------------------------------------


if (perl == TRUE) {
  perl <- "C:\\rtools42\\usr\\bin\\perl.exe"
}

###############################################################################
zz <- file("elaborazione.out", open="wt")
sink(zz, type="output")
###############################################################################

if (file.exists('modellimens_nuovi.txt')){
  
  frequenza <- 12
  
  # se si caricano gli indici da file CSV
  #dati <- read.table("indici.csv",header=TRUE,sep=";",dec=",")
  #dati <- ts(dati[,-1],freq=12,start=c(2003,1))
  
  # se invece si caricano gli indici da tabella SITIC
  indici  <- read.table("indicimens.txt",
                        colClasses=c(rep('character',4),rep('numeric',3),rep('character',6), rep('numeric',2),
                                     rep('character',1)),dec='.',sep=';',header=TRUE)
  
  # carica tabella di riepilogo
  riepilogo <- read.table("riepilogo.txt",
                          colClasses= c(rep('character',8),rep('numeric',6),rep('character',1),rep('numeric',1),
                                        rep('character',1)), dec='.',sep=';',header=TRUE)
  # carica modelli originali
  modelli_orig <- read.table("modellimens_orig.txt",
                             colClasses= c(rep('character',2),rep('numeric',1),rep('character',7),rep('numeric',8),
                                           rep('character',1),rep('numeric',1)), dec='.',sep=';',header=TRUE)
  
  # carica modelli nuovi
  modelli_nuovi <- read.table("modellimens_nuovi.txt",
                              colClasses= c(rep('character',2),rep('numeric',1),rep('character',7),rep('numeric',8),
                                            rep('character',1),rep('numeric',1)), dec='.',sep=';',header=TRUE)
  
  
  # elimina righe vuote e righe che iniziano con '#' da tabella modelli_nuovi
  modelli_nuovi <- modelli_nuovi[grep("^#",modelli_nuovi[,1],invert=T),]
  modelli_nuovi <- modelli_nuovi[grep("^$",modelli_nuovi[,1],invert=T),]
  
  # ordina per settore nomets e progressivo
  modelli_nuovi <- modelli_nuovi[order(modelli_nuovi$SERVIZIO,modelli_nuovi$COD_LIVELLO_1,modelli_nuovi$CODICE_1,modelli_nuovi$NOME_TS,modelli_nuovi$PROGRESSIVO),]
  
  # trasforma in minuscolo
  modelli_nuovi$PARAMETRO <- tolower(modelli_nuovi$PARAMETRO)
  # cambia versione TS
  modelli_nuovi$VERSIONE <- 4
  
  # seleziona nomi ts serie da destagionalizzare
  #if (is.null(mynamesmens)){ 
  #  mynamesmens <- union( unique(modelli_orig$NOME_TS) , unique(modelli_nuovi$NOME_TS))
  #} else {}
  
  # esegui solo se ci sono delle serie
  if(length(mynamesmens)>0){
    
    
    # scrivi file per TS con modelli orig
    con <- textConnection('tsinput',open='w')
    for (ii in mynamesmens){
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
    firstval <- grep("[Ii][Nn][Pp][Uu][Tt]",tsinput)[1]
    tsinput[firstval] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input iter=3 ",tsinput[firstval])
    write(tsinput,"serie.txt")
    
    # scrivi file per TS con modelli nuovi
    con <- textConnection('tsinput',open='w')
    for (ii in mynamesmens){
      #    print(which(mynamesmens==ii))
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
    firstval <- grep("input",tsinput)[1]
    tsinput[firstval] <- sub("input ","input iter=3 ",tsinput[firstval])
    tsinput <- sub("input ","input tabtables='xo,p,n,ca,s,u,pa,er,rg0,rgsa,stp,stn' tabtablet='xo,xi,xl,re,me,td,ee,to,ls,reg' ",tsinput)
    write(tsinput,"serienew.txt")
    
    
    # elimina cartelle preesistenti
    unlink('oldm',recursive=TRUE)
    unlink('oldgm',recursive=TRUE)
    unlink('newm',recursive=TRUE)
    unlink('newgm',recursive=TRUE)
    
    if(Sys.info()["sysname"]!='Linux'){
      # esegui ts - comandi DOS
      system(glue("{path_ts}TRAMO\\Tramo.exe -i serie.txt -o oldm -g oldgm"),ignore.stdout = FALSE,ignore.stderr = FALSE)
      system(glue("{path_ts}SEATS\\Seats.exe -i seats.itr -o oldm -g oldgm"),ignore.stdout = FALSE,ignore.stderr = FALSE)
      system(glue("{path_ts}TRAMO\\Tramo.exe -i serienew.txt -o newm -g newgm"),ignore.stdout = FALSE,ignore.stderr = FALSE) 
      system(glue("{path_ts}SEATS\\Seats.exe -i seats.itr -o newm -g newgm"),ignore.stdout = FALSE,ignore.stderr = FALSE) 
    } else {
      # esegui ts - comandi linux
      system("~/software/ts/942/tramo_lnx_32bit_942 -i serie.txt -o oldm -g oldgm",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("~/software/ts/942/seats_lnx_32bit_942 -i seats.itr -o oldm -g oldgm",ignore.stdout = TRUE,ignore.stderr = TRUE)
      system("~/software/ts/942/tramo_lnx_32bit_942 -i serienew.txt -o newm -g newgm",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("~/software/ts/942/seats_lnx_32bit_942 -i seats.itr -o newm -g newgm",ignore.stdout = TRUE,ignore.stderr = TRUE)
    }
    # elimina cartelle grafici
    unlink('oldgm',recursive=TRUE)
    unlink('newgm',recursive=TRUE)
    
    # carica output mensile
    vecchi <- output_tsplus("oldm",freq=frequenza)
    nuovi <- output_tsplus("newm",freq=frequenza)
    
    cat("##################################################################","\n\n")
    cat("SERIE MENSILI:","\n\n")
    cat("##################################################################","\n\n")
    
    cat("\n\n","Diagnostica modelli originali:","\n")
    print(vecchi$diagmat[,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])
    cat("\n\n","Diagnostica modelli nuovi:","\n")
    print(nuovi$diagmat[,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])
    
    cat("\n\n","********************************","\n")
    cat("Problemi nei modelli vecchi")
    cat("\n","********************************","\n\n")
    
    if (sum(vecchi$diagmat[,"QR.prob"]<0.05,na.rm=TRUE)>0){
      cat("\n","Statistica di LB significativa:","\n")
      print(vecchi$diagmat[vecchi$diagmat[,"QR.prob"]<=0.05 & !is.na(vecchi$diagmat[,"QR.prob"])
                           ,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 
    
    if (sum(vecchi$diagmat[,"N.prob"]<0.05,na.rm=TRUE)>0){
      cat("\n","Statistica di JB significativa:","\n")
      print(vecchi$diagmat[vecchi$diagmat[,"N.prob"]<=0.05 & !is.na(vecchi$diagmat[,"N.prob"]),c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 
    
    if (sum(vecchi$diagmat[,"QSR.prob"]<0.05,na.rm=TRUE) >0){
      cat("\n","Statistica di autocorrelazione stagionale significativa:","\n")
      print(vecchi$diagmat[vecchi$diagmat[,"QSR.prob"]<=0.05 & !is.na(vecchi$diagmat[,"QSR.prob"]),c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 
    
    if (sum(vecchi$diagmat[,"QR2.prob"]<0.05,na.rm=TRUE) >0){
      cat("\n","Statistica di LB sui quadrati dei residui significativa:","\n")
      print(vecchi$diagmat[vecchi$diagmat[,"QR2.prob"]<=0.05 & !is.na(vecchi$diagmat[,"QR2.prob"]),c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 
    
    if (sum(vecchi$diagmat[,"MODC"]=='Y') >0){
      cat("\n","Modelli cambiati in SEATS:","\n")
      print(vecchi$diagmat[vecchi$diagmat[,"MODC"]=='Y',c("TITLE")])} 
    
    if (sum(vecchi$diagmat[,"MODA"]=='Y') >0){
      cat("\n","Modelli approssimati:","\n")
      print(vecchi$diagmat[vecchi$diagmat[,"MODA"]=='Y',c("TITLE")])} 
    
    if (sum(vecchi$diagmat[,"ACF"]!='0') >0){
      cat("\n","Errore ACF:","\n")
      print(vecchi$diagmat[vecchi$diagmat[,"ACF"]!='0',c("TITLE")])} 
    
    if (sum(vecchi$diagmat[,"CCF"]!='0') >0){
      cat("\n","Errore CCF:","\n")
      print(vecchi$diagmat[vecchi$diagmat[,"CCF"]!='0',c("TITLE")])} 
    
    
    cat("\n\n","********************************","\n")
    cat("Problemi nei modelli nuovi")
    cat("\n","********************************","\n\n")
    
    if (sum(nuovi$diagmat[,"QR.prob"]<0.05,na.rm=TRUE)>0){
      cat("\n","Statistica di LB significativa:","\n")
      print(
        nuovi$diagmat[nuovi$diagmat[,"QR.prob"]<=0.05 & !is.na(nuovi$diagmat[,"QR.prob"])
                      ,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 
    
    if (sum(nuovi$diagmat[,"N.prob"]<0.05,na.rm=TRUE)>0){
      cat("\n","Statistica di JB significativa:","\n")
      print(nuovi$diagmat[nuovi$diagmat[,"N.prob"]<=0.05 & !is.na(nuovi$diagmat[,"N.prob"]),c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 
    
    if ( sum(nuovi$diagmat[,"QSR.prob"]<0.05,na.rm=TRUE) >0){
      cat("\n","Statistica di autocorrelazione stagionale significativa:","\n")
      print(nuovi$diagmat[nuovi$diagmat[,"QSR.prob"]<=0.05 & !is.na(nuovi$diagmat[,"QSR.prob"]),c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 
    
    if ( sum(nuovi$diagmat[,"QR2.prob"]<0.05,na.rm=TRUE) >0){
      cat("\n","Statistica di LB sui quadrati dei residui significativa:","\n")
      print(nuovi$diagmat[nuovi$diagmat[,"QR2.prob"]<=0.05 & !is.na(nuovi$diagmat[,"QR2.prob"]),c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")])} 
    
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
    
    
    
    pdf("confronto_mens_orig_nuovi.pdf")
    if(length(mynamesmens)!=1){
      for (ii in mynamesmens){
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
      for (ii in mynamesmens){
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
    
    ###
    pdf("confronto_mens_orig_nuovi_WDA.pdf")
    if(length(mynamesmens)!=1){
      for (ii in mynamesmens){
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
      for (ii in mynamesmens){
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
    
    ###
    
    
    
    pdf("mens_new.pdf",paper='a4r',width=0)
    for (ii in 1:length(nuovi$nomi)){
      plot(nuovi,ii,vecchi)
    }
    dev.off()
    
    pdf("mens_old.pdf",paper='a4r',width=0)
    for (ii in 1:length(vecchi$nomi)){
      plot(vecchi,ii)
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
    
    # scrivi file modelli
    if (diffmod==TRUE){
      modelli_finali <- diffmodelli(modelli_nuovi,modelli_orig)
    } else {
      modelli_finali <- modelli_nuovi
    }
    
    
    
    
    # esporta serie in formato csv
    
    if(outputdata==TRUE){
      write.csv2(x=nuovi$serie_grezza,file="grezzi.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
      write.csv2(x=nuovi$serie_sa,file="sa_new.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
      write.csv2(x=nuovi$serie_wd,file="wda_new.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
      write.csv2(x=vecchi$serie_sa,file="sa_old.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
      # da versione nuova
      write.csv2(x=vecchi$serie_wd,file="wda_old.csv",row.names=paste(trunc(time(nuovi$serie_grezza)),cycle(nuovi$serie_grezza),sep='q'))
    }
  }
}
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
  #if (is.null(mynamestrim)){ 
  #  mynamestrim <- union( unique(modelli_orig$NOME_TS) , unique(modelli_nuovi$NOME_TS))
  #} else {}
  
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
    firstval <- grep("[Ii][Nn][Pp][Uu][Tt]",tsinput)[1]
    tsinput[firstval] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input iter=3 ",tsinput[firstval])
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
    firstval <- grep("[Ii][Nn][Pp][Uu][Tt]",tsinput)[1]
    tsinput[firstval] <- sub("[Ii][Nn][Pp][Uu][Tt] ","input iter=3 ",tsinput[firstval])
    write(tsinput,"serienew.txt")
    
    
    # elimina cartelle preesistenti
    unlink('oldt',recursive=TRUE)
    unlink('oldgt',recursive=TRUE)
    unlink('newt',recursive=TRUE)
    unlink('newgt',recursive=TRUE)
    
    if(Sys.info()["sysname"]!='Linux'){
      # esegui ts - comandi DOS
      system(glue("{path_ts}TRAMO\\Tramo.exe -i serie.txt -o oldt -g oldgt"),ignore.stdout = FALSE,ignore.stderr = FALSE) 
      system(glue("{path_ts}SEATS\\Seats.exe -i seats.itr -o oldt -g oldgt"),ignore.stdout = FALSE,ignore.stderr = FALSE)
      system(glue("{path_ts}TRAMO\\Tramo.exe -i serienew.txt -o newt -g newgt"),ignore.stdout = FALSE,ignore.stderr = FALSE) 
      system(glue("{path_ts}SEATS\\Seats.exe -i seats.itr -o newt -g newgt"),ignore.stdout = FALSE,ignore.stderr = FALSE)
    } else {
      # esegui ts - comandi linux
      system("tramo_10 -i serie.txt -o oldt -g oldgt",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("seats_10 -i seats.itr -o oldt -g oldgt",ignore.stdout = TRUE,ignore.stderr = TRUE)
      system("tramo_10 -i serienew.txt -o newt -g newgt",ignore.stdout = TRUE,ignore.stderr = TRUE) 
      system("seats_10 -i seats.itr -o newt -g newgt",ignore.stdout = TRUE,ignore.stderr = TRUE)
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
    
    
    
    pdf("confronto_trim_orig_nuovi.pdf")
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
      #plot(nuovi,ii)
      plot(nuovi,ii,vecchi,quarterly = TRUE) 
      #plot(nuovi,ii,vecchi,quarterly = TRUE, last_percentage_vars=30) # per forzare il numero di variazioni percentuali da mostrare, a partire da fine serie
      
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
    
    # vecchia versione
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

prepare_JDplus_file("serienew.txt", "serienew_M.txt")
prepare_JDplus_file("serie.txt", "serie_M.txt")

sink(file=NULL) # chiudo elaborazione.out


# AUTO-COMMENT -----------------------------------------------------------------

elaborazione_commented <- "elaborazione_commented.out"
if (!file.exists(elaborazione_commented)) {
  file.create(elaborazione_commented)
}

if(auto_comment_enable)
{  
    file.copy( "elaborazione.out", elaborazione_commented, overwrite = TRUE)
    zz <- file("elaborazione_commented.out", open="at") # apre in APPEND
    sink(zz, type="output")
    
    cat("\n\n","********************************","\n")
    cat("Informazioni sui modelli:")
    cat("\n","********************************","\n\n")
    
    if ( nrow(nuovi$diagmat) >0)
    {
      
      m_or_t = ""
      if(length(mynamesmens)>length(mynamestrim))  {
        m_or_t = "m"
      }else{
        m_or_t = "t"
      }
      
      old_series_outlier_dictionary = create_all_series_outlier_dictionary(file=glue("old{m_or_t}\\toutlier.m"))
      new_series_outlier_dictionary = create_all_series_outlier_dictionary(file=glue("new{m_or_t}\\toutlier.m"))
      change_in_external_regressors = FALSE
      
      df_new_models = read_tfit_file(glue("new{m_or_t}\\tfit.m"))
      df_old_models = read_tfit_file(glue("old{m_or_t}\\tfit.m"))
      
      df_new_params = read_diagnostic_file(glue("new{m_or_t}\\tarmapar.m"))
      df_old_params = read_diagnostic_file(glue("old{m_or_t}\\tarmapar.m"))
      
      df_calend_new = read_diagnostic_file(glue("new{m_or_t}\\tcalend.m"))
      df_calend_old = read_diagnostic_file(glue("old{m_or_t}\\tcalend.m"))
      
      df_reg_new = read_diagnostic_file(glue("new{m_or_t}\\tregvar.m"))
      df_reg_old = read_diagnostic_file(glue("old{m_or_t}\\tregvar.m"))
      
      df_ttest_new = read_diagnostic_file(glue("new{m_or_t}\\ttest.m"))
      df_ttest_old = read_diagnostic_file(glue("old{m_or_t}\\ttest.m"))
      
      first_model_diff = TRUE
      comment = ""
      
      for(titolo in nuovi$diagmat$TITLE)
      {
        
        new_model = df_new_models[df_new_models$TITLE==paste0("\"",titolo,"\""),]
        old_model = df_old_models[df_old_models$TITLE==paste0("\"",titolo,"\""),]
        
        regressors_old = extract_regressors_from_string(df_reg_old[df_reg_old$TITLE==titolo,]$params_string)
        regressors_new = extract_regressors_from_string(df_reg_new[df_reg_new$TITLE==titolo,]$params_string)
        
        regressors_variations = FALSE
        # if(length(regressors_new[[1]]) != length(regressors_old[[1]])) {regressors_variations=TRUE} 
        if(length(regressors_new) != length(regressors_old)) {regressors_variations=TRUE} 
        easter_variations = (new_model$EE  != old_model$EE)
        
        model_variations = (new_model$Lam!=old_model$Lam || new_model$Mean!=old_model$Mean ||
                              new_model$P  !=old_model$P   || new_model$D !=old_model$D   || new_model$Q !=old_model$Q ||
                              new_model$BP !=old_model$BP  || new_model$BD!=old_model$BD  || new_model$BQ!=old_model$BQ ||
                              easter_variations || regressors_variations)
        
        
        comment = paste(comment, "\n\t\t", titolo, "\n")
        comment = paste(comment, "Modello originale:\n###\n")
        diagnostica_originale     = vecchi$diagmat[vecchi$diagmat[,"TITLE"] == titolo, c("BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")]
        diagnostica_originale_str = paste(diagnostica_originale, collapse = "\t")
        comment = paste0(comment, "###" , diagnostica_originale_str, "\n")
        
        
        #vecchi$diagmat[vecchi$diagmat[,"N.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")]
        #nuovi$diagmat[nuovi$diagmat[,"N.prob"]<=0.05,c("TITLE","BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")]
        comment = paste0(comment, "###", print_dictionary_values(old_series_outlier_dictionary[titolo]), "\n")
        #comment = paste0(comment, "###\n###", outlier_variations, "\n")
        
        if(model_variations || always_print_full_model_comment)
        {
          tarmapar_header = "  MEAN      (t)       PHI1       (t)       PHI2       (t)       PHI3       (t)       BPHI       (t)        TH1       (t)        TH2       (t)        TH3       (t)        BTH       (t)"
          comment = paste0(comment, "###" , tarmapar_header, "\n")
          comment = paste0(comment, "###" , df_old_params[df_old_params$TITLE==titolo,]$params_string, "\n")
          
          if(length(regressors_old)>0)
          {
            #comment = paste0(comment, "###" , regressors_old[[1]], "\n")
            comment = paste0(comment, "###" , df_reg_old[df_reg_old$TITLE==titolo,]$params_string, "\n")
          }   
        } 
        
        # stampa la Pasqua solo se ci sono variazioni
        # if((easter_variations && old_model$EE==1) || always_print_full_model_comment) # la pasqua c'era nel modello vecchio perci? ? stata tolta nel nuovo
        # {
        #   easter_effect_old = df_calend_old[df_calend_old$TITLE==titolo,]$params_string
        #   easter_effect_old = get_EE(easter_effect_old)
        #   
        #   comment = paste0(comment, "###  EE:" , easter_effect_old, "\n")
        # }
        if(old_model$EE==1) # stampa la Pasqua sempre, se c'?
        { 
          easter_effect_old = df_calend_old[df_calend_old$TITLE==titolo,]$params_string
          easter_effect_old = get_EE(easter_effect_old)
          comment = paste0(comment, "###  EE:" , easter_effect_old, "\n")
        }
        
        
        comment = paste(comment, "\nModello nuovo:\n")
        outlier_variations = compare_dicts(old_dict = old_series_outlier_dictionary[[titolo]], new_series_outlier_dictionary[[titolo]])
        
        if(!model_variations && outlier_variations=="")  # pu? dare problemi se si sostituisce un regressore con un altro (nel caso togliere questo if e lasciare il codice sottostante fuori dal blocco else (non essendoci if non servir? scrivere else))
        {
          comment = paste0(comment, "\t\t\t\t" , "nessuna variazione da modello originale", "\n")
        }
        else
        {
          comment = paste0(comment,"###\n")
          if(outlier_variations != "" || easter_variations || regressors_variations) 
          {
            comment = paste0(comment, "###")
            if(outlier_variations != "")
            {
              comment = paste0(comment, outlier_variations)
            }
            if(easter_variations)
            {
              #easter_effect_old = df_old_params[df_old_params$TITLE==titolo,]$params_string
              #easter_effect_old = sub(".*\\)", "", easter_effect_old)
  
              if(new_model$EE==0 && old_model$EE==1)
              {
                easter_effect_old = df_calend_old[df_calend_old$TITLE==titolo,]$params_string
                add_or_remove_EE="-" 
                EE_value = get_EE(easter_effect_old)
              }
              else #(new_model$EE==1 && old_model$EE==0)
              {
                easter_effect_new = df_calend_new[df_calend_new$TITLE==titolo,]$params_string
                add_or_remove_EE="+" 
                EE_value = get_EE(easter_effect_new)
              }
              
              comment = paste0(comment, add_or_remove_EE ,"EE:" , EE_value, ";")
            }
            if(regressors_variations)
            {
              comment = paste0(comment, " Variazioni nei regressori")
            }   
            comment = paste0(comment,"\n")
          }
          
          diagnostica_nuova     = nuovi$diagmat[nuovi$diagmat[,"TITLE"] == titolo, c("BIC","QR.prob","QR2.prob","QSR.prob","N.prob","N.test","TVA","TVC")]
          diagnostica_nuova_str = paste(diagnostica_nuova, collapse = "\t")
          old_model_fit = extract_fit_from_string( df_ttest_old[df_ttest_old$TITLE==titolo,]$params_string )
          new_model_fit = extract_fit_from_string( df_ttest_new[df_ttest_new$TITLE==titolo,]$params_string )
          
          
          comment = paste0(comment, "###" , diagnostica_nuova_str, "\t\t", old_model_fit, "->", new_model_fit ,"\n")
          comment = paste0(comment, "###" , print_dictionary_values(new_series_outlier_dictionary[titolo]), "\n")
          
          if(model_variations || always_print_full_model_comment)
          {
            tarmapar_header = "  MEAN      (t)       PHI1       (t)       PHI2       (t)       PHI3       (t)       BPHI       (t)        TH1       (t)        TH2       (t)        TH3       (t)        BTH       (t)"
            comment = paste0(comment, "###" , tarmapar_header, "\n")
            specifications = df_new_params[df_new_params$TITLE==titolo,]$params_string
            comment = paste0(comment, "###" , specifications, "\n")
            
            if(length(regressors_new)>0)
            {
              comment = paste0(comment, "###" , df_reg_new[df_reg_new$TITLE==titolo,]$params_string, "\n")
            } 
          }
          
          #if((new_model$EE==1 && always_print_full_model_comment) || (new_model$EE==1 && new_model$EE==0)) # versione che non stampa EE se variano solo outliers
          if(new_model$EE==1)
          { 
            easter_effect_new = df_calend_new[df_calend_new$TITLE==titolo,]$params_string
            EE_value = get_EE(easter_effect_new)
            comment = paste0(comment, "###  EE:" , EE_value, "\n")
          }
        }
        
      }
      cat(comment)
  }
} else # auto_comment_enable==FALSE
{
  zz <- file("elaborazione_commented.out", open="wt") # apre in SOVRASCRITTURA
  sink(zz, type="output")
  cat("Abilitare auto-comment per scrivere questo file")
}


sink(file=NULL) # close elaborazione_commented.out

###########################

closeAllConnections()



# files_to_send <- c("elaborazione.out", "elaborazione_commented.out", "sa_new.csv", "sa_new_trim.csv", "newt/toutlier.m", "newm/toutlier.m", "confronto_trim_orig_nuovi.pdf", "confronto_mens_orig_nuovi.pdf", "modelli_finali.txt", "modellitrim_nuovi.txt",  "modellimens_nuovi.txt") # all files version
files_to_send <- c("elaborazione.out", "elaborazione_commented.out", "modelli_finali.txt")

if(!is.null(mynamesmens) && length(mynamesmens)>0){
  files_to_send <- c(files_to_send, c("sa_new.csv", "newm/toutlier.m", "confronto_mens_orig_nuovi.pdf", "modellimens_nuovi.txt", "mens_new.pdf"))
  files_to_send <- c(files_to_send, c("newm/tcalend.m", "newm/tregvar.m", "newm/tarmapar.m", "newm/ttest.m"))
}
if(!is.null(mynamestrim) && length(mynamestrim)>0){
  files_to_send <- c(files_to_send, c("sa_new_trim.csv" , "newt/toutlier.m", "confronto_trim_orig_nuovi.pdf", "modellitrim_nuovi.txt", "trim_new.pdf"))
  files_to_send <- c(files_to_send, c("newt/tcalend.m", "newt/tregvar.m", "newt/tarmapar.m", "newt/ttest.m"))
}  

make_files_to_send_folder(folder_name = "toSend" , to_send = files_to_send, begin_cleaning_folder = TRUE)



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





