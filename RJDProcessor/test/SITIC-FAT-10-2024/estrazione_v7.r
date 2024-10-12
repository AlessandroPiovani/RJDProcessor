# setwd("U:\\Desktop\\SITIC-VEE\\")
setwd("C:\\Users\\UTENTE\\Desktop\\RJDopenCruncher\\RJDProcessor\\test\\SITIC-FAT\\")


zz <- file("estrazione.out", open="wt")
sink(zz, type="output")
options(width=210)
options(max.print = .Machine$integer.max)
# Estrazione dati
# occorre avere installato sqlplus
# e avere un file estrai.sql con i criteri di estrazione


shell('sqlplus -s UP_ALESSANDRO_PIOVANI/Peterbill2025@EXAPES @estrai.sql')

# legge i file CSV scaricati
indici <- read.table("INDICI.CSV",dec=".",sep=";",header=TRUE,strip.white = TRUE,
                     colClasses=c(rep('character',4),rep('integer',3),rep('character',6),rep('numeric',2),'character'))
indicisa <- read.table("INDICISA.CSV",dec=".",sep=";",header=TRUE,strip.white = TRUE,
                       colClasses=c(rep('character',4),rep('integer',3),rep('character',6),rep('numeric',2),'character'))
modelli <-   read.table(  "MODELLI.CSV",dec=".",sep=";",header=TRUE,strip.white = TRUE,
                        colClasses=c(rep('character',2),'integer',rep('character',7),rep('integer',8),'character','integer'))
riepilogo <- read.table("RIEPILOGO.CSV",dec=".",sep=";",header=TRUE,strip.white = TRUE,
                        colClasses=c(rep('character',8),rep('integer',6),'character',rep('integer',2)))


# elimina duplicati (dati rivisti)
#cat("indici:",dim(indici),"\n")
#massimo <- aggregate(formula=TIPO_IND~+SERVIZIO+VARIABILE+CATEGORIA_IND+ANNO_BASE+ANNO+MESE+CLASS_1+COD_LIV_1+CODICE_1+CLASS_2+COD_LIV_2+CODICE_2,data=indici,FUN=max)
#indici <- merge(indici,massimo)
#indici <- indici[order(indici$SERVIZIO,indici$VARIABILE,indici$ANNO_BASE,indici$CLASS_1,indici$COD_LIV_1,indici$CODICE_1,indici$CLASS_2,indici$COD_LIV_2,indici$CODICE_2,indici$ANNO,indici$MESE),]
#cat("indici:",dim(indici),"\n\n")

# elimina duplicati (dati rivisti)
#cat("indicisa:",dim(indicisa),"\n")
#massimo <- aggregate(formula=TIPO_IND~+SERVIZIO+VARIABILE+CATEGORIA_IND+ANNO_BASE+ANNO+MESE+CLASS_1+COD_LIV_1+CODICE_1+CLASS_2+COD_LIV_2+CODICE_2,data=indicisa,FUN=max)
#indicisa <- merge(indicisa,massimo)
## ordina
#indicisa <- indicisa[order(indicisa$SERVIZIO,indicisa$VARIABILE,indicisa$ANNO_BASE,indicisa$CLASS_1,indicisa$COD_LIV_1,indicisa$CODICE_1,indicisa$CLASS_2,indicisa$COD_LIV_2,indicisa$CODICE_2,indicisa$ANNO,indicisa$MESE),]
#cat("indicisa:",dim(indicisa),"\n\n")

# seleziona modelli aperti
cat("modelli:",dim(modelli),"\n\n")
modelli <- modelli[is.na(modelli$ANNO_FIN),]
 #ordina
modelli <- modelli[order(modelli$COD_LIVELLO_1,modelli$CODICE_1,modelli$COD_LIVELLO_2,modelli$CODICE_2,modelli$NOME_TS,modelli$PROGRESSIVO) ,]
cat("modelli:",dim(modelli),"\n\n")

# seleziona riepilogo aperti
cat("riepilogo:",dim(riepilogo),"\n\n")
riepilogo <- riepilogo[is.na(riepilogo$ANNO_FIN),]
 #ordina
riepilogo <- riepilogo[order(riepilogo$SERVIZIO,riepilogo$VARIABILE,riepilogo$ANNO_BASE,riepilogo$CLASS_1,riepilogo$COD_LIV_1,riepilogo$CODICE_1,riepilogo$CLASS_2,riepilogo$COD_LIV_2,riepilogo$CODICE_2) ,]
cat("riepilogo:",dim(riepilogo),"\n\n")
#


# controlla la coerenza tra tabella riepilogo dove APPROCCIO='DIR' e tabella modelli
riepilogodir <- riepilogo[riepilogo$APPROCCIO=='DIR',c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","ANNO_BASE")]
modellidir <- modelli[modelli$PROGRESSIVO==10,c("SERVIZIO","VARIABILE","CLASS_1","COD_LIVELLO_1","CODICE_1","CLASS_2","COD_LIVELLO_2","CODICE_2","ANNO_BASE")]

colnames(riepilogodir) <- c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","ANNO_BASE")
colnames(modellidir) <- c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","ANNO_BASE")

# merge dei due data frame
# la colonna myidm contiene NA dove mancano il modello
# la colonna myidr contiene NA dove mancano la riga riepilogo
merged <- merge(cbind(modellidir,myidm=1:nrow(modellidir)),cbind(riepilogodir,myidr=1:nrow(riepilogodir)),all=TRUE)

if ( sum(is.na(merged))>0) {
   cat('################################################################################','\n\n')
   cat('ERRORE:: Incoerenza tra tabella modelli e tabella riepilogo','\n\n')
   if ( sum(is.na(merged$myidr))>0) {
      cat('Ci sono i seguenti modelli apparentemente inutili:','\n')
      print(merged[is.na(merged$myidr),1:9])
   }
   if ( sum(is.na(merged$myidm))>0){
      cat('\n','Mancano i modelli per le serie:','\n')
      print(merged[is.na(merged$myidm),1:9])
      cat('################################################################################','\n\n')
   }
}


# dividi indici mensili e trimestrali grezzi
temp <- merge(indici,riepilogo[,c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","FREQUENZA")],all.x=TRUE)
indicimens <- temp[temp$FREQUENZA==12,names(indici)]
indicitrim <- temp[temp$FREQUENZA==4,names(indici)]


# dividi indici mensili e trimestrali sa
temp <- merge(indicisa,riepilogo[,c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","FREQUENZA")],all.x=TRUE)
indicisamens <- temp[temp$FREQUENZA==12,names(indici)]
indicisatrim <- temp[temp$FREQUENZA==4,names(indici)]

# dividi modelli mensili e trimestrali
temp <- merge(modelli,riepilogo[,c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2","FREQUENZA")],
              by.x=c("SERVIZIO","VARIABILE","CLASS_1","COD_LIVELLO_1","CODICE_1","CLASS_2","COD_LIVELLO_2","CODICE_2"),
              by.y=c("SERVIZIO","VARIABILE","CLASS_1","COD_LIV_1","CODICE_1","CLASS_2","COD_LIV_2","CODICE_2"),
              all.x=TRUE)
modellimens <- temp[temp$FREQUENZA==12,names(modelli)]
modellitrim <- temp[temp$FREQUENZA==4,names(modelli)]

nomi_ts_trim <- paste0("\"", unique(gsub("\\s+", "", modellitrim$NOME_TS)) , "\"", collapse = ", ")
nomi_ts_mens <- paste0("\"", unique(gsub("\\s+", "", modellimens$NOME_TS)) , "\"", collapse = ", ")

cat('################################################################################','\n\n')

cat("Ci sono", length(nomi_ts_mens) , "serie mensili da destagionalizzare in questo gruppo di serie:","\n")
#cat("Ci sono", sum(modellimens$PROGRESSIVO==10) , "serie mensili da destagionalizzare in questo gruppo di serie:","\n")
if(nomi_ts_mens!="\"\"") cat("\n\t", nomi_ts_mens,"\n")

cat("\n\nCi sono", length(nomi_ts_trim), "serie trimestrali da destagionalizzare in questo gruppo di serie:","\n")
#cat("\n\nCi sono", sum(modellitrim$PROGRESSIVO==10), "serie trimestrali da destagionalizzare in questo gruppo di serie:","\n")
if(nomi_ts_trim!="\"\"") cat("\n\t", nomi_ts_trim, "\n\n")

cat('################################################################################','\n\n')

# scrivi i file con indici, modelli, riepilogo
# all'inizio modelli_nuovi e modelli_vecchi coincidono

if (sum(modellimens$PROGRESSIVO==10)>0){
   write.table(modellimens,file="modellimens_orig.txt",sep=";",quote=F,row.names=F)
   write.table(modellimens,file="modellimens_nuovi.txt",sep=";",quote=F,row.names=F)
   write.table(indicimens,file="indicimens.txt",sep=";",quote=F,row.names=F)
   write.table(indicisamens,file="indicisamens.txt",sep=";",quote=F,row.names=F)
}

if (sum(modellitrim$PROGRESSIVO==10)>0){
   write.table(modellitrim,file="modellitrim_orig.txt",sep=";",quote=F,row.names=F)
   write.table(modellitrim,file="modellitrim_nuovi.txt",sep=";",quote=F,row.names=F)
   write.table(indicitrim,file="indicitrim.txt",sep=";",quote=F,row.names=F)
   write.table(indicisatrim,file="indicisatrim.txt",sep=";",quote=F,row.names=F)
}

write.table(riepilogo,file="riepilogo.txt",sep=";",quote=F,row.names=F)
#

######### controlla quali serie non sono destagionalizzate
#

if (nrow(riepilogo[riepilogo$AGGIUSTAMENTO>4,])>0){
   cat('################################################################################','\n\n')
   cat('\n\n','Ci sono', length(riepilogo[riepilogo$AGGIUSTAMENTO>4,])  ,'serie destagionalizzate copiate dalla grezza:','\n\n')
   print(riepilogo[riepilogo$AGGIUSTAMENTO>4,])

}
cat('################################################################################','\n\n')

closeAllConnections()
