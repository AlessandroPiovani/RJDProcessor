###############################################################################
#
# crea un file in input per JDemetra+, leggibile tramite l'apposito plugin.
#
# Il nuovo file è creato adattando quello preparato per Tramo-Seats.
# Le modifiche apportate sono: caratteri tutti maiuscoli, il file è creato
# nella cartella regr e non contiene il riferimento alla cartella /regr nelle 
# righe dove prende in ingresso i regressori.
#  
# input    input_file: file, con connessione già aperta! 
#          output_file: nome del file di JD+ che verrà creato
#
#
#
# Copyright Alessandro Piovani 2023 - alessandro.piovani@istat.it
#
###############################################################################


prepare_JDplus_file <- function(input_file, output_file){
  # leggi il contenuto del file di input

  
  input_text <- readLines(input_file)

  # crea la cartella "regr" se non esiste già
  dir.create("regr", showWarnings = FALSE)
  
  # crea il percorso completo del file di output nella cartella "regr"
  output_path <- file.path("regr", output_file)
  
  # crea il file di output
  output <- file(output_path, "w")
  
  # scorri tutte le righe del file di input
  for (line in input_text) {
    # converti tutti i caratteri alfabetici in maiuscolo e rimuovi la sottostringa "regr/"
    line_upper <- gsub("REGR/", "", toupper(line))
    
    # scrivi la riga nel file di output
    writeLines(line_upper, output)
  }
  
  # chiudi i file
  close(output)
  #close(input_file)
  return()
}





###############################################################################
#
# crea una successione di date
#
# input    startdate: data di inizio
#          enddate:  data di fine
#          frequency: frequenza
#
# output   lista con successione di date comprese tra startdate e enddate
#
# Copyright Giancarlo Bruno 2015 - gbruno@istat.it
#
###############################################################################
seqdate <- function(startdate,enddate,freq){
   # return a list with dates between startdate and enddate
   serie <- ts(start=startdate,end=enddate,frequency=freq)
   serie <- lapply(c(time(serie)), function(x) c(trunc(x), round(((x-trunc(x))*freq+1),0)))
   return(serie)
}



###############################################################################
#
# seleziona indici da nome TS
#
# input    nome: stringa con nome ts
#          modelli:  data frame con tabella modelli SITIC
#          indici:  data frame con tabella indici SITIC
#          siticdate:  limita l'estensione della serie secondo
#                      quello che c'e nella tabella modelli
#
# output   serie storica da indici SITIC
#
# Copyright Giancarlo Bruno 2014 - gbruno@istat.it
#
###############################################################################
seriesitic <- function(nome,modelli,indici,riepilogo,siticdate=TRUE){
   tempid <- modelli[modelli$NOME_TS==nome & modelli$PROGRESSIVO==10,
                     c("SERVIZIO","VARIABILE",
                       "CLASS_1","COD_LIVELLO_1","CODICE_1",
                       "CLASS_2","COD_LIVELLO_2","CODICE_2",
                       "ANNO_BASE")]
   names(tempid) <- c("SERVIZIO","VARIABILE",
                      "CLASS_1","COD_LIV_1","CODICE_1",
                      "CLASS_2","COD_LIV_2","CODICE_2",
                      "ANNO_BASE")
   if(nrow(tempid)==0) {
      cat(" per la serie",nome,"non e' definito un modello nella tabella MODELLI","\n")
      return()
   } else {
   tempriepilogo <- merge(tempid,riepilogo)
   #  aggiungi controllo che ci sia una sola riga
   #  tempts <- merge(tempid,indici)

   tempts <- indici[indici$SERVIZIO==tempid$SERVIZIO &
   indici$VARIABILE==tempid$VARIABILE &
   indici$CLASS_1==tempid$CLASS_1 &
   indici$COD_LIV_1==tempid$COD_LIV_1 &
   indici$CODICE_1==tempid$CODICE_1 &
   indici$CLASS_2==tempid$CLASS_2 &
   indici$COD_LIV_2==tempid$COD_LIV_2 &
   indici$CODICE_2==tempid$CODICE_2 &
   indici$ANNO_BASE==tempid$ANNO_BASE,]

   #  aggiungi controllo che ci siano i dati
   tempts <- tempts[order(tempts$ANNO,tempts$MESE),]
   frequenza  <- tempriepilogo$FREQUENZA
   dates <- data.frame(t(simplify2array( 
                                        seqdate( c(head(tempts$ANNO,n=1),head(tempts$MESE,n=1)) ,c(tail(tempts$ANNO,n=1),tail(tempts$MESE,n=1)),freq=frequenza)
                                        )))
   names(dates) <- c("ANNO","MESE")
   tempts <- merge(dates,tempts,all.x=TRUE)
   tempts <- tempts[order(tempts$ANNO,tempts$MESE),]
   tempts <- ts(tempts[,"INDICE"],start=
                c(head(tempts$ANNO,n=1),head(tempts$MESE,n=1))
                ,frequency=frequenza)
   if(siticdate==TRUE){
      startts <- unlist(modelli[modelli$NOME_TS==nome & modelli$PROGRESSIVO==10, c("ANNO_SER","MESE_SER")])
      tempts <- window(tempts,start=startts)
   } else {}
   tspar <- modelli[modelli$NOME_TS==nome,c("PROGRESSIVO","PARAMETRO")]
   tspar <- tspar[order(tspar$PROGRESSIVO),"PARAMETRO"]

   # avvertimento valori mancanti
   if (sum(is.na(tempts))>0){
      cat("****************************",
          "\n","La serie ",nome, "contiene dei valori mancanti","\n\n")
   } else {}
   return(list(siticts =tempts,tspar=tspar))
   }
}
###############################################################################


###############################################################################
#
# seleziona indici da identificativo SITIC
#
# input    id: data frame di una riga contenente i campi:
#                              "SERVIZIO",
#                              "VARIABILE"
#                              "CLASS_1"
#                              "COD_LIV_1"
#                              "CODICE_1"
#                              "CLASS_2"
#                              "COD_LIV_2"
#                              "CODICE_2"
#                              "ANNO_BASE"
#                              "FREQUENZA"
#           indici: tabella indici; devono essere statre eliminate le revisioni e 
#                       deve contenere una sola tipologia di indici (ad esempio solo destagionalizzati)
#
# output   serie storica da indici SITIC
#
# Copyright Giancarlo Bruno 2015 - gbruno@istat.it
#
###############################################################################
seriesitic2 <- function(id,indici){
   #  aggiungi controllo sui campi
   #  tempts <- merge(tempid,indici)

   tempts <- indici[indici$SERVIZIO==id$SERVIZIO &
   indici$VARIABILE==id$VARIABILE &
   indici$CLASS_1==id$CLASS_1 &
   indici$COD_LIV_1==id$COD_LIV_1 &
   indici$CODICE_1==id$CODICE_1 &
   indici$CLASS_2==id$CLASS_2 &
   indici$COD_LIV_2==id$COD_LIV_2 &
   indici$CODICE_2==id$CODICE_2 &
   indici$ANNO_BASE==id$ANNO_BASE,]

   tempts <- tempts[order(tempts$ANNO,tempts$MESE),]
   nomets <- paste(id[c(2,4,5,7,8)],collapse=".")
  #  controllo che ci siano i dati
   if (nrow(tempts)==0){
     cat("\n\n","La serie ", nomets,"non presenta osservazioni","\n\n")
   return()}
   frequenza  <- id$FREQUENZA
   dates <- data.frame(t(simplify2array( 
                                        seqdate( c(head(tempts$ANNO,n=1),head(tempts$MESE,n=1)) ,c(tail(tempts$ANNO,n=1),tail(tempts$MESE,n=1)),freq=frequenza)
                                        )))
   names(dates) <- c("ANNO","MESE")
   tempts <- merge(dates,tempts,all.x=TRUE)
   tempts <- tempts[order(tempts$ANNO,tempts$MESE),]
   tempts <- ts(tempts[,"INDICE"],start=
                c(head(tempts$ANNO,n=1),head(tempts$MESE,n=1))
                ,frequency=frequenza)
   return(list(serie=tempts,nomeserie=nomets))
   }

###############################################################################
#
# seleziona identificativi SITIC da nomi TS
#
# input    nomi: vettore di stringhe con nomi ts
#          modelli:  data frame con tabella modelli SITIC
#
# output   ident: data frame con identificativi e nomi TS
#          mancanti: vettore di stringhe con nomi mancanti
#
# Copyright Giancarlo Bruno 2013 - gbruno@istat.it
#
###############################################################################
nomits_toid <- function(nomi,modelli){
   # inizializza
   ident <- NULL
   mancanti <- NULL
   # per ogni nome verifica se c'e' un record nella tabella modelli
   # e incrementa ident, altrimenti incrementa mancanti
   for (ii in nomi){
      tempid <- modelli[modelli$NOME_TS==ii & modelli$PROGRESSIVO==10,
                        c("SERVIZIO","VARIABILE",
                          "CLASS_1","COD_LIVELLO_1","CODICE_1",
                          "CLASS_2","COD_LIVELLO_2","CODICE_2",
                          "ANNO_BASE","NOME_TS")]
      if (nrow(tempid)==0) { 
         mancanti <- c(mancanti,ii)} else {ident <- rbind(ident,tempid[1,])}
   }
   return(list(ident=ident,mancanti=mancanti))
}
###############################################################################

###############################################################################
#
# seleziona nomi TS da identificativi SITIC
#
# input    ident: data frame con identificativi SITIC (almeno i 9 campi
#          per definire univocamente una serie storica in SITIC
#          modelli: data frame con tabella modelli SITIC
#
# output   tsident: data frame con identificativi e nomi TS
#          mancanti: data frame con identificativi ai quali non
#          corrisponde un campo NOME_TS nella tabella modelli
#
# Copyright Giancarlo Bruno 2013 - gbruno@istat.it
#
###############################################################################
id_tonomits <- function(ident,modelli){
   # inizializza
   tsident <- data.frame(SERVIZIO=character(),
                         VARIABILE=character(),
                         ANNO_BASE=numeric(),
                         CLASS_1=character(),
                         COD_LIVELLO_1=character(),
                         CODICE_1=character(),
                         CLASS_2=character(),
                         COD_LIVELLO_2=character(),
                         CODICE_2=character(),
                         NOME_TS=character())
   mancanti <- tsident
   # per ogni riga di ident verifica se c'e' un nome ts nella tabella modelli.
   # In caso affermativo incrementa di un record tsident, altrimenti mancanti.
   for (ii in 1:nrow(ident)){
      tempident <- ident[ii,]
      tempnomits <-modelli[
                           modelli$SERVIZIO==tempident$SERVIZIO &
                           modelli$VARIABILE==tempident$VARIABILE &
                           modelli$ANNO_BASE==tempident$ANNO_BASE &
                           modelli$CLASS_1==tempident$CLASS_1 &
                           modelli$COD_LIVELLO_1==tempident$COD_LIV_1 &
                           modelli$CODICE_1==tempident$CODICE_1 &
                           modelli$CLASS_2==tempident$CLASS_2 &
                           modelli$COD_LIVELLO_2==tempident$COD_LIV_2 &
                           modelli$CODICE_2==tempident$CODICE_2,
                           c("SERVIZIO","VARIABILE",
                             "CLASS_1","COD_LIVELLO_1","CODICE_1",
                             "CLASS_2","COD_LIVELLO_2","CODICE_2",
                             "ANNO_BASE","NOME_TS")]
      if (nrow(tempnomits)==0) { 
         mancanti <- rbind(mancanti,tempident)
         } else {tsident <- rbind(tsident,tempnomits[1,])
      }
   }
   return(list(ident=tsident,mancanti=mancanti))
}
###############################################################################


###############################################################################
#
# confronta due tabelle modelli producendo una tabella di differenze
#
# modelli_nuovi: data frame con modelli nuovi
# modelli_orig: data frame con modelli originali
#
# output: data frame con modelli diversi
#
# Copyright Giancarlo Bruno 2015 - gbruno@istat.it
#
###############################################################################
diffmodelli <- function(modelli_nuovi,modelli_orig){

   #ordina per settore nomets e progressivo
   modelli_nuovi <- modelli_nuovi[order(modelli_nuovi$SERVIZIO,
                                        modelli_nuovi$VARIABILE,
                                        modelli_nuovi$CLASS_1,
                                        modelli_nuovi$COD_LIVELLO_1,
                                        modelli_nuovi$CODICE_1,
                                        modelli_nuovi$CLASS_2,
                                        modelli_nuovi$COD_LIVELLO_2,
                                        modelli_nuovi$CODICE_2,
                                        modelli_nuovi$ANNO_BASE,
                                        modelli_nuovi$ANNO_INI,
                                        modelli_nuovi$NOME_TS,modelli_nuovi$PROGRESSIVO),]

   modelli_orig <- modelli_orig[order(modelli_orig$SERVIZIO,
                                      modelli_orig$VARIABILE,
                                      modelli_orig$CLASS_1,
                                      modelli_orig$COD_LIVELLO_1,
                                      modelli_orig$CODICE_1,
                                      modelli_orig$CLASS_2,
                                      modelli_orig$COD_LIVELLO_2,
                                      modelli_orig$CODICE_2,
                                      modelli_orig$ANNO_BASE,
                                      modelli_orig$ANNO_INI,
                                      modelli_orig$NOME_TS,modelli_orig$PROGRESSIVO),]

   tempo <- split(modelli_orig,modelli_orig$NOME_TS)
   tempn <- split(modelli_nuovi,modelli_nuovi$NOME_TS)

   if (length(setdiff(names(tempn),names(tempo)))!=0){
      cat("\n\n","*********","\n","Serie in modelli nuovi non presenti in modelli orig","\n")
      cat(setdiff(names(tempn),names(tempo)),sep="\n")
   } else {}
   if (length(setdiff(names(tempo),names(tempn)))!=0){
      cat("\n\n","*********","\n","Serie in modelli orig non presenti in modelli nuovi","\n\n")
      cat(setdiff(names(tempo),names(tempn)),sep="\n")
      cat("\n\n","*********","\n")
   } else {}

   # serie da confrontare: quelle comuni 
   confronti <- intersect(names(tempo),names(tempn))
   # costruisci elenco con le precedenti piu' quelle presenti solo in modelli nuvoi
   tempcheck <- c(confronti,setdiff(names(tempn),names(tempo)))
   # marca con numero >0 tutte
   tempcheck <- cbind(tempcheck,1)
   rownames(tempcheck) <- tempcheck[,1]

   # check: se modelli uguali per quelle comuni il marcatore uguale a 0
   for (ii in confronti){
      if ( nrow(tempo[[ii]]) != nrow(tempn[[ii]])) next 
      tempcheck[ii,2] <- sum( tempn[[ii]]!=tempo[[ii]] ,na.rm=TRUE)
   }
   # seleziona una lista solo con modelli variati o nuovi
   selected <- tempcheck[tempcheck[,2]>0,1]

   tempnn <- tempn[selected]
   # ricostruisci un data frame con i modelli variati o nuovi
   tempf <- NULL
   if (length(tempnn)>0){
      for (ii in 1:length(tempnn)){
         tempf <- rbind(tempf,tempnn[[ii]])
      }
   } else {
      cat("\n\n","*********","\n","Non ci sono modelli nuovi","\n")
   }

   return(tempf)

}
