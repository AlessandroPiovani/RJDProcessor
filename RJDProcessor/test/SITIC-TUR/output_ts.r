output_ts <- function(x,...)  UseMethod("output_ts")

output_ts.default <- function(x,freq=12) {
   require(tframe)
   require(gdata)
   #   UseMethod("outputts")
   nomefile <- paste(x,"tfit.m",sep="/")
   #   print(nomefile)
   tfit <- read.table(nomefile,header=TRUE,dec=".")
   tfit$TITLE <- as.character(tfit$TITLE)
   nomefile <- paste(x,"sgeneral.m",sep="/")
   #   print(nomefile)
   sgeneral  <- read.table(nomefile,header=FALSE,skip=4,dec='.',
                           colClasses= c(rep('integer',1),rep('character',4),rep('integer',7),rep('numeric',1),rep('character',7))
                           )
   nomefile <- paste(x,"sparami.m",sep="/")
   #   print(nomefile)
   sparami  <- read.table(nomefile,header=FALSE,skip=5,dec='.',
                          colClasses= c(rep('integer',1),rep('character',1),rep('numeric',14))
                          )
   nomefile <- paste(x,"sparamii.m",sep="/")
   #   print(nomefile)
   sparamii  <- read.table(nomefile,header=FALSE,skip=5,dec='.',
                           colClasses= c(rep('integer',1),rep('character',1),rep('numeric',4),rep('integer',3),rep('numeric',2))
                           )

   # nome del file
   nomefile <- paste(x,"tarmapar.m",sep="/")
   # leggi il file
   tarmapar <- readLines(nomefile)
   # elimina la prima riga
   tarmapar <- tarmapar[-c(1)]
   # elimina le parentesi
   tarmapar <- gsub('\\(|\\)','',tarmapar)
   # elimina  \"
   tarmapar <- gsub('\\"','',tarmapar)
   # elimina spazi iniziali
   tarmapar <- gsub('^[[:space:]]+','',tarmapar)
   # dividi mediante uno o pi? spazi
   tarmapar <- strsplit(tarmapar,'[[:space:]]+')
   # trasforma in una matrice
   tarmapar <- simplify2array(tarmapar,higher=TRUE)
   # dividi l'oggetto in stringhe e resto
   tempnomi <- tarmapar[2,]
   tempdata <- tarmapar[-c(1,2),,drop=FALSE]
   # trasforma in numeri tempdata
   tempdata <- ifelse((tempdata=='-' | tempdata=='*****'),NA,tempdata)
   tempdata <- matrix(as.numeric(tempdata),nrow(tempdata))
   tarmapar <- data.frame(TITLE=tempnomi,t(tempdata))

   # nome del file
   nomefile <- paste(x,"tdeterm.m",sep="/")
   # leggi il file
   tdeterm <- readLines(nomefile)
   # elimina la prima riga
   tdeterm <- tdeterm[-c(1)]
   # elimina le parentesi
   tdeterm <- gsub('\\(|\\)','',tdeterm)
   # elimina  \"
   tdeterm <- gsub('\\"','',tdeterm)
   # elimina spazi iniziali
   tdeterm <- gsub('^[[:space:]]+','',tdeterm)
   # dividi mediante uno o pi? spazi
   tdeterm <- strsplit(tdeterm,'[[:space:]]+')
   # seleziona solo le serie con la media
   #tdeterm <- tdeterm[tfit$Mean==1]
   # trasforma in una matrice
   #   tdeterm <- simplify2array(tdeterm)
   # dividi l'oggetto in stringhe e resto
   tempnomi <- simplify2array(lapply(tdeterm,FUN=function(x){x[2]}))
   tempdata <- simplify2array(lapply(tdeterm,FUN=function(x){x[11:12]}))
   # trasforma in numeri tempdata
   tempdata <- matrix(as.numeric(tempdata),nrow(tempdata))
   tdeterm <- data.frame(TITLE=tempnomi,t(tempdata))

   nomi <- as.character(tfit[,"TITLE"])
   #nomefile <- paste(x,"tparams.m",sep="/")
   #tparams <- read.table(nomefile,header=T,dec=".")
   #nomi <- as.character(tfit[,"TITLE"])

   sgeneral <- sgeneral[,-1]
   colnames(sgeneral) <- c("TITLE","preadj","modchange","modapprox","mod_m","mod_p","mod_d","mod_q","mod_bp","mod_bd","mod_bq","SDres","specfac","checkacf","checkccf","detcom_tc","detcomp_s","detcomp_u","modif_trans")

   sparami <- sparami[,-1]
   colnames(sparami) <- c("TITLE","sdinn_tc","sdinn_s","sdinn_trans","sdinn_u","sdinn_sa","seest_conc_tc","seest_conc_sa","serev_conc_tc","serev_conc_sa","stderr_rg_1_tc","stderr_rg_1_sa","stderr_rgmm_x","stderr_rgmm_tc","stderr_rgmm_sa")

   sparamii <- sparamii[,-1]
   colnames(sparamii) <- c("TITLE","conv_tc_1y","conv_sa_1y","conv_tc_5y","conv_sa_5y","sigseas_hist","sigseas_prel","sigseas_forc","daa_tc","daa_sa")

   # rispetto al nuovo manca la media
   colnames(tarmapar) <- c("TITLE","TVP.phi1","TVP.phi1.t","TVP.phi2","TVP.phi2.t","TVP.phi3","TVP.phi3.t","TVP.bphi1","TVP.bphi1.t" ,"TVP.th1","TVP.th1.t","TVP.th2","TVP.th2.t","TVP.th3","TVP.th3.t","TVP.bth1","TVP.bth1.t")

   colnames(tdeterm) <- c("TITLE","TVP.m","TVP.m.t")

   #  acquisisci output di SEATS

   nomefile <- paste(x,"table-s.out",sep="/")
   con <- file(nomefile,"r",encoding="latin1")

   serie_grezza <- NULL
   serie_sa <- NULL
   serie_trend <- NULL
   serie_stochsa <- NULL
   serie_stochtrend <- NULL
   serie_wd <- NULL
   serie_linear <- NULL
   serie_irreg <- NULL
   determ_eff <- NULL
   serie_totseas <- NULL
   serie_stochseas <- NULL
   serie_si <- NULL

   # contiene i numeri delle variabili selezionate
   selected <- NULL
   # contiene i nomi delle variabili selezionate
   finalnames <- NULL

   for (ii in 1:length(nomi)){
      #         print(ii)
      temp_string <- nomi[ii]
      counter <- 0
      repeat{
         string <- readLines(con=con,n=1)
         if (trim(sub("[(].*","",gsub('"','',string))) == temp_string) break
         counter <- counter+1
      }
      nn <- tfit[ii,'Nz']
      tempser <- read.table(file=con,skip=1,header=F,nrows=nn)
      # seleziona la colonna delle date
      temp <- as.numeric(unlist(strsplit(as.character(tempser[,1]),split="-")))
      # separa l'anno dai periodi e imposta la frequenza al massimo dei periodi
      frequenza <- max(temp[1:length(temp)%%2==1])
      if(frequenza!=freq){next} else {
         selected <- c(selected,ii)
         finalnames <- c(finalnames,temp_string)
      }
      datainizio <- rev(as.numeric(unlist(strsplit(as.character(tempser[1,1]),split="-"))))
      tempser <- ts(tempser[,-1],start=datainizio,frequency=freq)

      serie_grezza <- tbind(serie_grezza,tempser[,1])
      serie_sa <- tbind(serie_sa,tempser[,3])
      serie_trend <- tbind(serie_trend,tempser[,2])
      serie_stochsa <- tbind(serie_stochsa,tempser[,12])
      serie_stochtrend <- tbind(serie_stochtrend,tempser[,11])
      serie_totseas <- tbind(serie_totseas,tempser[,1]-tempser[,3])
      serie_irreg <- tbind(serie_irreg,tempser[,6])
      if (tfit[tfit[,"TITLE"]==nomi[ii],"Lam"]==1){
         serie_wd <- tbind(serie_wd,tempser[,1]-tempser[,5])
         serie_linear <- tbind(serie_linear,tempser[,1]-tempser[,7])
         determ_eff <- tbind(determ_eff,tempser[,7]-tempser[,5])
         serie_stochseas <- tbind(serie_stochseas,tempser[,1]-tempser[,7]-tempser[,12]) # linear - stoch sa
         serie_si <- tbind(serie_si,tempser[,1]-tempser[,7]-tempser[,12]+tempser[,6]) # stochseas + irreg
      } else {
         serie_wd <- tbind(serie_wd,tempser[,1]/tempser[,5]*100)
         serie_linear <- tbind(serie_linear,tempser[,1]/tempser[,7]*100)
         determ_eff <- tbind(determ_eff,tempser[,7]/tempser[,5]*100)
         serie_stochseas <- tbind(serie_stochseas,tempser[,1]/tempser[,7]/tempser[,12]*10000) # linear / stoch sa
         serie_si <- tbind(serie_si,tempser[,1]/tempser[,7]/tempser[,12]*tempser[,6]*100) # stochseas * irreg
      }
   }

   close(con)

   #  acquisisci output di TRAMO

   nomefile <- paste(x,"table-t.out",sep="/")
   con <- file(nomefile,"r",encoding="latin1")

   residui <- NULL

   #  loop solo sulle serie selezionate precedentemente
   for (ii in 1:length(finalnames)){
      #   print(ii)
      temp_string <- finalnames[ii]
      counter <- 0
      repeat{
         string <- readLines(con=con,n=1)
         if (trim(sub("[(].*","",gsub('"','',string))) == temp_string) break
         counter <- counter+1
      }
      nn <- tfit[tfit$TITLE==temp_string,'Nz']
      tempser <- read.table(file=con,skip=1,header=F,nrows=nn)
      # seleziona la colonna delle date
      temp <- as.numeric(unlist(strsplit(as.character(tempser[,1]),split="-")))
      datainizio <- rev(as.numeric(unlist(strsplit(as.character(tempser[1,1]),split="-"))))
      tempser <- ts(tempser[,-1],start=datainizio,frequency=freq)

      residui <- tbind(residui,tempser[,4])
   }

   close(con)

   residui <- ifelse(residui==0,NA,residui)
   #  acquisisci gradi di liberta per test di Ljung-Box

   ordine <- rep(NA,length(finalnames))
   statistica <- rep(NA,length(finalnames))

   for (ii in 1:length(finalnames)){
      temp_number <- selected[ii]
      temp_string <- finalnames[ii]
      nomefile <- paste('^t',temp_number,'_.*out',sep="")
      nomefile <- paste(x,dir(x)[grep(nomefile,dir(x))],sep='/')
      con <- file(nomefile,"r",encoding="latin1")
      #   print(ii)
      counter <- 0
      repeat{
         string <- readLines(con=con,n=1)
         if (grepl('LJUNG-BOX',string)) break
         counter <- counter+1
      }
      # separa gli elementi della riga dove c'e' la stringa LJUNG-BOX
      temp <- unlist(strsplit(trim(string),'[[:space:]]+'))
      ordine[ii]  <-  as.numeric(temp[6])
      statistica[ii] <- as.numeric(temp[8])
      close(con)
   }

   if(length(finalnames)==1){
      names(serie_grezza) <- finalnames
      names(serie_sa) <- finalnames
      names(serie_trend) <- finalnames
      names(serie_stochsa) <- finalnames
      names(serie_stochtrend) <- finalnames
      names(serie_wd) <- finalnames
      names(serie_linear) <- finalnames
      names(serie_irreg) <- finalnames
      names(serie_totseas) <- finalnames
      names(serie_stochseas) <- finalnames
      names(serie_si) <- finalnames
      names(determ_eff) <- finalnames
      names(residui) <- finalnames
   } else {
      colnames(serie_grezza) <- finalnames
      colnames(serie_sa) <- finalnames
      colnames(serie_trend) <- finalnames
      colnames(serie_stochsa) <- finalnames
      colnames(serie_stochtrend) <- finalnames
      colnames(serie_wd) <- finalnames
      colnames(serie_linear) <- finalnames
      colnames(serie_irreg) <- finalnames
      colnames(serie_totseas) <- finalnames
      colnames(serie_stochseas) <- finalnames
      colnames(serie_si) <- finalnames
      colnames(determ_eff) <- finalnames
      colnames(residui) <- finalnames
   }


   if (freq==12){
      tfit$LBprob <- round(pchisq(q=tfit$Q.val,df=c(freq*2-tfit$P-tfit$Q-tfit$BP-tfit$BQ),lower.tail=FALSE),3)
      tfit$LBsqprob <- round(pchisq(q=tfit$Q2,df=c(freq*2),lower.tail=FALSE),3)
   } else if (freq==4) {
      tfit$LBprob <- round(pchisq(q=tfit$Q.val,df=c(freq*3-tfit$P-tfit$Q-tfit$BP-tfit$BQ),lower.tail=FALSE),3)
      tfit$LBsqprob <- round(pchisq(q=tfit$Q2,df=c(freq*3),lower.tail=FALSE),3)
   }

   # calcola correttamente i gdl dei test di autocorrelazione dei residui
   tfit$Q.val2 <- rep(0,length(tfit$LBprob))
   tfit$Q.ord2 <- rep(0,length(tfit$LBprob))
   tfit$LBprob2 <- rep(0,length(tfit$LBprob))
   tfit$LBsqprob2 <- rep(0,length(tfit$LBprob))
   tfit$Q.val2[selected] <- statistica
   tfit$Q.ord2[selected] <- ordine
   tfit$LBprob2[selected] <- round(pchisq(q=tfit$Q.val2[selected],df= c( ordine -tfit$P[selected] -tfit$Q[selected]- tfit$BP[selected]-tfit$BQ[selected]),lower.tail=FALSE),3)
   tfit$LBsqprob2[selected] <- round(pchisq(q=tfit$Q2[selected],df= ordine,lower.tail=FALSE),3)

   # calcola altre prob
   tfit$BPprob <- round(pchisq(q=tfit$QS,df=c(2),lower.tail=FALSE),3)
   tfit$NORMprob <- round(pchisq(q=as.double(as.character(tfit$N.test)),df=2,lower.tail=FALSE),3)
   # inserisci Nprob=0 dove Ntest ? NA
   tfit$NORMprob <- ifelse(is.na(tfit$NORMprob),0,tfit$NORMprob)

   tfit <- tfit[tfit[,"TITLE"] %in% finalnames,]

   temp <- merge(tfit,sgeneral,by='TITLE',sort=FALSE)
   temp <- merge(temp,sparami,by='TITLE',sort=FALSE)
   temp <- merge(temp,sparamii,by='TITLE',sort=FALSE)
   temp <- merge(temp,tarmapar,by='TITLE',sort=FALSE)
   temp <- merge(temp,tdeterm,by='TITLE',sort=FALSE)

   diagmat <- temp[,c('TITLE','Nz','Lam','Mean','P','D','Q','BP','BD','BQ',
                      'TVP.m','TVP.m.t','TVP.phi1','TVP.phi1.t','TVP.phi2','TVP.phi2.t','TVP.phi3','TVP.phi3.t','TVP.bphi1','TVP.bphi1.t',
                      'TVP.th1','TVP.th1.t','TVP.th2','TVP.th2.t','TVP.th3','TVP.th3.t','TVP.bth1','TVP.bth1.t',
                      'SE.res.','BIC','Q.val2','Q.ord2','LBprob2','Q2','Q.ord2','LBsqprob2',
                      'N.test','NORMprob','SK.t.','KUR.t.','QS','BPprob','modchange','modapprox','checkacf','checkccf',
                      'sdinn_tc','sdinn_s','sdinn_trans','sdinn_u','sdinn_sa','seest_conc_tc','seest_conc_sa',
                      'stderr_rg_1_tc','stderr_rg_1_sa',
                      'conv_tc_1y','conv_sa_1y','conv_tc_5y','conv_sa_5y','sigseas_hist','sigseas_prel','sigseas_forc','daa_tc','daa_sa')]
   colnames(diagmat) <- c('TITLE','OBS','LOG','ORD.m','ORD.p','ORD.d','ORD.q','ORD.bp','ORD.bd','ORD.bq',
                          'TVP.m','TVP.m.t','TVP.phi1','TVP.phi1.t','TVP.phi2','TVP.phi2.t','TVP.phi3','TVP.phi3.t','TVP.bphi1','TVP.bphi1.t',
                          'TVP.th1','TVP.th1.t','TVP.th2','TVP.th2.t','TVP.th3','TVP.th3.t','TVP.bth1','TVP.bth1.t',
                          'SER','BIC','QR.val','QR.ord','QR.prob','QR2.val','QR2.ord','QR2.prob',
                          'N.test','N.prob','TVA','TVC','QSR.val','QSR.prob','MODC','MODA','ACF','CCF',
                          'SEI.tc','SEI.s','SEI.trans','SEI.u','SEI.sa','SES.tc','SES.sa',
                          'SETCC.tc','SETCC.sa',
                          'CONV.1ytc','CONV.1ysa','CONV.5ytc','CONV.5ysa','SIGNS.hist','SIGNS.prel','SIGNS.forc','DAA.tc','DAA.sa')

   out <- NULL

   out$temp <- temp
   out$diagmat <- diagmat
   out$serie_grezza <- serie_grezza
   out$serie_sa <- serie_sa
   out$serie_trend <- serie_trend
   out$serie_stochsa <- serie_stochsa
   out$serie_stochtrend <- serie_stochtrend
   out$serie_wd <- serie_wd
   out$serie_linear <- serie_linear
   out$serie_irreg <- serie_irreg
   out$serie_totseas <- serie_totseas
   out$determ_eff <- determ_eff
   out$residui <- residui
   out$tfit <- tfit
   out$nomi <- finalnames
   out$sgeneral <- sgeneral
   out$sparami <- sparami
   out$sparamii <- sparamii
   out$tarmapar <- tarmapar
   out$tdeterm <- tdeterm
   out$serie_stochseas <- serie_stochseas
   out$serie_si <- serie_si

   class(out) <- "output_ts"

   out
}






plot.output_ts <- function(x,i,...){
   if (length(x$nomi)>1){
      oldpar <- par(no.readonly = T)
      require(tframe)
      nomi <- x$nomi
      newx <- x[c("serie_grezza","serie_sa","serie_wd","serie_linear","serie_totseas","determ_eff","serie_irreg","serie_stochseas","serie_si")]
      xvalid <- lapply(newx,function(x,i) trimNA(x[,i]),i)

      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      plot(xvalid$serie_grezza,main='raw and wda',ylab='',xlab='',las=1);lines(xvalid$serie_wd,col=2)
      plot(xvalid$serie_grezza,main='raw and sa',ylab='',xlab='',las=1);lines(xvalid$serie_sa,col=2)
      plot(xvalid$serie_totseas,main='seasonality',ylab='',xlab='',las=1)
      #   plot(dati[,tempname],main='serie aggiustata',ylab='',xlab='',las=1);lines(serie_linear,col=2)
      if(diff(range( 
                    xvalid$determ_eff[!is.na(xvalid$determ_eff)]
                    ))<1){
         #plot(xvalid$determ_eff,main='outliers',ylim=range(xvalid$determ_eff)+c(-1,1),ylab='',xlab='',las=1)
         plot(xvalid$determ_eff,main='outliers',ylab='',xlab='',las=1)
      } else {
         plot(xvalid$determ_eff,main='outliers',ylab='',xlab='',las=1)
      }
      mtext(line=0,nomi[i],outer=T,font=2,cex=1.2)
      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      {
         monthplot(xvalid$serie_si,type='h',lwd.base=0,main="SI ratio")
         monthplot(xvalid$serie_stochseas,type='l',lwd.base=0,lwd=2,add=TRUE,col=2)
      }
      spectrum(cbind(xvalid$serie_grezza,xvalid$serie_sa),col=c(1,2),spans=c(3,3),main='periodogram')
      legend(x='topright',legend=c('raw series','sa series'),lty=c(1,1),col=c(1,2),inset=0.01)
      mtext(line=0,nomi[i],outer=T,font=2,cex=1.2)
      par <- oldpar
   } else {
      oldpar <- par(no.readonly = T)
      require(tframe)
      nomi <- x$nomi
      newx <- x[c("serie_grezza","serie_sa","serie_wd","serie_linear","serie_totseas","determ_eff","serie_irreg","serie_stochseas","serie_si")]
      xvalid <- lapply(newx,function(x) trimNA(x))

      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      plot(xvalid$serie_grezza,main='raw and wda',ylab='',xlab='',las=1);lines(xvalid$serie_wd,col=2)
      plot(xvalid$serie_grezza,main='raw and sa',ylab='',xlab='',las=1);lines(xvalid$serie_sa,col=2)
      plot(xvalid$serie_totseas,main='seasonality',ylab='',xlab='',las=1)
      #   plot(dati[,tempname],main='serie aggiustata',ylab='',xlab='',las=1);lines(serie_linear,col=2)
      if(diff(range( 
                    xvalid$determ_eff[!is.na(xvalid$determ_eff)]
                    ))<1){
         plot(xvalid$determ_eff,main='outliers',ylim=range(xvalid$determ_eff)+c(-1,1),ylab='',xlab='',las=1)
      } else {
         plot(xvalid$determ_eff,main='outliers',ylab='',xlab='',las=1)
      }
      mtext(line=0,nomi[i],outer=T,font=2,cex=1.2)
      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      {
         monthplot(xvalid$serie_si,type='h',lwd.base=0,main="SI ratio")
         monthplot(xvalid$serie_stochseas,type='l',lwd.base=0,lwd=2,add=TRUE,col=2)
      }
      spectrum(cbind(xvalid$serie_grezza,xvalid$serie_sa),col=c(1,2),spans=c(3,3),main='periodogramma')
      legend(x='topright',legend=c('serie grezza','serie destagionalizzata'),lty=c(1,1),col=c(1,2),inset=0.01)
      mtext(line=0,nomi[i],outer=T,font=2,cex=1.2)
      par <- oldpar
   }
}



output_tsplus <- function(x,...)  UseMethod("output_tsplus")

output_tsplus.default <- function(x,freq=12) {
   require(tframe)
   require(gdata)
   require(stringr)
   #   UseMethod("outputts")
   nomefile <- paste(x,"tfit.m",sep="/")
   tfit  <- read.table(nomefile,header=FALSE,skip=2,dec='.',na.strings='-',
                      colClasses= c(rep('integer',1),rep('character',1),rep('integer',12),rep('numeric',2),rep('character',3))
                      )
   nomefile <- paste(x,"ttest.m",sep="/")
   ttest  <- read.table(nomefile,header=FALSE,skip=3,dec='.',na.strings='-',
                      colClasses= c(rep('integer',1),rep('character',1),rep('numeric',2),rep('character',1),rep('numeric',4),
                                    rep('numeric',4),rep('integer',1),
                                    rep('character',1),rep('numeric',1),rep('character',1),rep('numeric',1))
                      )
   nomefile <- paste(x,"TsignifS.m",sep="/")
   TsignifS  <- read.table(nomefile,header=FALSE,skip=2,dec='.',na.strings='-',
                      colClasses= c(rep('integer',1),rep('character',1),rep('numeric',4),rep('character',5),rep('numeric',1))
                      )
   nomefile <- paste(x,"sgeneral.m",sep="/")
   sgeneral  <- read.table(nomefile,header=FALSE,skip=3,dec='.',na.strings='-',
                      colClasses= c(rep('character',6),rep('numeric',2),rep('integer',7),rep('numeric',9))
                      )
   # elimina carattere ^ che a volte appare nel numero della serie in sgeneral
   sgeneral[,1] <- as.numeric(sub('\\^','',sgeneral[,1]))

   nomefile <- paste(x,"sparami.m",sep="/")
   sparami  <- read.table(nomefile,header=FALSE,skip=4,dec='.',na.strings='-',
                      colClasses= c(rep('character',2),rep('numeric',17))
                      )
   # elimina carattere ^ che a volte appare nel numero della serie in sgeneral
   sparami[,1] <- as.numeric(sub('\\^','',sparami[,1]))

   nomefile <- paste(x,"sparamii.m",sep="/")
   sparamii  <- read.table(nomefile,header=FALSE,skip=4,dec='.',na.strings='-',fill=TRUE,
                      colClasses= c(rep('character',2),rep('integer',1),rep('numeric',6),rep('character',7))
                      )
   # elimina carattere ^ che a volte appare nel numero della serie in sgeneral
   sparamii[,1] <- as.numeric(sub('\\^','',sparamii[,1]))

   nomefile <- paste(x,"sparami3.m",sep="/")
   sparami3  <- read.table(nomefile,header=FALSE,skip=3,dec='.',na.strings='-',
                      colClasses= c(rep('character',2),rep('numeric',2),rep('character',2),rep('integer',4),
                        rep('numeric',1),rep('character',1))
                      )
   # elimina carattere ^ che a volte appare nel numero della serie in sgeneral
   sparami3[,1] <- as.numeric(sub('\\^','',sparami3[,1]))

   # nome del file
   nomefile <- paste(x,"tarmapar.m",sep="/")
   # leggi il file
   tarmapar <- readLines(nomefile)
   # elimina le prime 2 righe
   tarmapar <- tarmapar[-c(1:2)]
   # elimina le parentesi
   tarmapar <- gsub('\\(|\\)','',tarmapar)
   # elimina  \"
   tarmapar <- gsub('\\"','',tarmapar)
   # elimina spazi iniziali
   tarmapar <- gsub('^[[:space:]]+','',tarmapar)
   # dividi mediante uno o pi? spazi
   tarmapar <- strsplit(tarmapar,'[[:space:]]+')
   # trasforma in una matrice
   tarmapar <- simplify2array(tarmapar)
   # dividi l'oggetto in stringhe e resto
   tempnomi <- tarmapar[2,]
   tempdata <- tarmapar[-c(1,2),,drop=FALSE]
   # trasforma in numeri tempdata
   tempdata <- ifelse((tempdata=='-' | tempdata=='*****'),NA,tempdata)
   tempdata <- matrix(as.numeric(tempdata),nrow(tempdata))
   tarmapar <- data.frame(TITLE=tempnomi,t(tempdata))


   colnames(tfit) <- c("n","TITLE","MQ","Nz","RemObs","MO","Lam","Mean","P","D","Q","BP","BD","BQ","SE.res.","BIC","OUT","TD","EE")
   nomi <- toupper(as.character(tfit[,"TITLE"]))
   #nomefile <- paste(x,"tparams.m",sep="/")
   #tparams <- read.table(nomefile,header=T,dec=".")
   #nomi <- as.character(tfit[,"TITLE"])

   ttest <- ttest[,-1]
   colnames(ttest) <- c("TITLE","tmean","Q.val","N.test","SK.t.","KUR.t.","Q2","RUNS","meanstab","varstab","meanstablast","varstablast","CloseTD",
                        "levlog","OofS_F","modelfit","Fmodel")
   TsignifS <- TsignifS[,-1]
   colnames(TsignifS) <- c("TITLE","QSLin","QS","SEAS_NPLin","SEAS_NPRes","seasPeakLin","seasPeakRes","sigSeasLin","sigSeasRes","FtestSeasLin","FtestSeasRes")


   sgeneral <- sgeneral[,-1]
   colnames(sgeneral) <- c("TITLE","preadj","modchange","modapprox","qual","ksd","kqstat","mod_m","mod_p","mod_d","mod_q","mod_bp","mod_bd","mod_bq","mean","phi1","phi2","phi3","bphi","th1","th2","th3","bth")

   sparami <- sparami[,-1]
   colnames(sparami) <- c("TITLE","sdinn_a","sdinn_tc","sdinn_s","sdinn_trans","stoc_td","sdinn_u","sdinn_sa","seest_conc_tc","seest_conc_sa","serev_conc_tc","serev_conc_sa","stderr_rg_1_tc","stderr_rg_1_sa","stderr_rgmm_x","stderr_rgmm_tc","stderr_rgmm_sa","SIGD")

   sparamii <- sparamii[,-1]
   colnames(sparamii) <- c("TITLE","spec_factor","conv_tc_1y","conv_sa_1y","conv_tc_5y","conv_sa_5y","bias_tc","bias_sa","checkacf","checkccf","detcom_tc","detcomp_s","detcomp_u","modif_trans","modif_sa")
   sparami3 <- sparami3[,-1]
   colnames(sparami3) <- c("TITLE","QS2","NP","spec","overall","sigseas_hist","sigseas_prel","sigseas_forc","persistnum","persisttest","saquest")

   colnames(tarmapar) <- c("TITLE","TVP.m","TVP.m.t","TVP.phi1","TVP.phi1.t","TVP.phi2","TVP.phi2.t","TVP.phi3","TVP.phi3.t","TVP.bphi1","TVP.bphi1.t" ,"TVP.th1","TVP.th1.t","TVP.th2","TVP.th2.t","TVP.th3","TVP.th3.t","TVP.bth1","TVP.bth1.t")

tfit[,'TITLE'] <- toupper(tfit[,'TITLE'])
ttest[,'TITLE'] <- toupper(ttest[,'TITLE'])
tarmapar[,'TITLE'] <- toupper(tarmapar[,'TITLE'])
TsignifS[,'TITLE'] <- toupper(TsignifS[,'TITLE'])
sgeneral[,'TITLE'] <- toupper(sgeneral[,'TITLE'])
sparami[,'TITLE'] <- toupper(sparami[,'TITLE'])
sparamii[,'TITLE'] <- toupper(sparamii[,'TITLE'])
sparami3[,'TITLE'] <- toupper(sparami3[,'TITLE'])

   #  acquisisci output di SEATS

   nomefile <- paste(x,"table-s.out",sep="/")
   con <- file(nomefile,"r",encoding="latin1")

   serie_grezza <- NULL
   serie_sa <- NULL
   serie_trend <- NULL
   serie_stochsa <- NULL
   serie_stochtrend <- NULL
   serie_wd <- NULL
   serie_linear <- NULL
   serie_irreg <- NULL
   determ_eff <- NULL
   serie_totseas <- NULL
   serie_stochseas <- NULL
   serie_si <- NULL

   # contiene i numeri delle variabili selezionate
   selected <- NULL
   # contiene i nomi delle variabili selezionate
   finalnames <- NULL

   for (ii in 1:length(nomi)){
      #         print(ii)
      temp_string <- nomi[ii]
      counter <- 0
      repeat{
         string <- readLines(con=con,n=1)
         if ( word(tolower(trim(sub("[(].*","",gsub('"','',string))))) == tolower(temp_string)) break
         counter <- counter+1
      }
      nn <- tfit[ii,'Nz']
      tempser <- read.table(file=con,skip=1,header=F,nrows=nn)
      # seleziona la colonna delle date
      temp <- as.numeric(unlist(strsplit(as.character(tempser[,1]),split="-")))
      # separa l'anno dai periodi e imposta la frequenza al massimo dei periodi
      frequenza <- max(temp[1:length(temp)%%2==1])
      if(frequenza!=freq){next} else {
         selected <- c(selected,ii)
         finalnames <- c(finalnames,temp_string)
      }
      datainizio <- rev(as.numeric(unlist(strsplit(as.character(tempser[1,1]),split="-"))))
      tempser <- ts(tempser[,-1],start=datainizio,frequency=freq)

      serie_grezza <- tbind(serie_grezza,tempser[,1])
      serie_sa <- tbind(serie_sa,tempser[,3])
      serie_trend <- tbind(serie_trend,tempser[,2])
      serie_stochsa <- tbind(serie_stochsa,tempser[,12])
      serie_stochtrend <- tbind(serie_stochtrend,tempser[,11])
      serie_totseas <- tbind(serie_totseas,tempser[,5])
      serie_irreg <- tbind(serie_irreg,tempser[,6])
      if (tfit[tfit[,"TITLE"]==nomi[ii],"Lam"]==1){
         serie_wd <- tbind(serie_wd,tempser[,4])
         serie_linear <- tbind(serie_linear,tempser[,1]-tempser[,7])
         determ_eff <- tbind(determ_eff,tempser[,4]-tempser[,1]+tempser[,7])
         serie_stochseas <- tbind(serie_stochseas,tempser[,1]-tempser[,7]-tempser[,12]) # linear - stoch sa
         serie_si <- tbind(serie_si,tempser[,1]-tempser[,7]-tempser[,12]+tempser[,6]) # stochseas + irreg
      } else {
         serie_wd <- tbind(serie_wd,tempser[,4])
         serie_linear <- tbind(serie_linear,tempser[,1]/tempser[,7]*100)
         determ_eff <- tbind(determ_eff,tempser[,4]/tempser[,1]*tempser[,7]/100)
         serie_stochseas <- tbind(serie_stochseas,tempser[,1]/tempser[,7]/tempser[,12]*100) # linear - stoch sa
         serie_si <- tbind(serie_si,tempser[,1]/tempser[,7]/tempser[,12]*tempser[,6]) # stochseas + irreg
      }

   }

   close(con)

   #  acquisisci output di TRAMO

   nomefile <- paste(x,"table-t.out",sep="/")
   con <- file(nomefile,"r",encoding="latin1")

   residui <- NULL

   #  loop solo sulle serie selezionate precedentemente
   for (ii in 1:length(finalnames)){
      #   print(ii)
      temp_string <- finalnames[ii]
      counter <- 0
      repeat{
         string <- readLines(con=con,n=1)
         if ( word(tolower(trim(sub("[(].*","",gsub('"','',string))))) == tolower(temp_string)) break
         counter <- counter+1
      }
      nn <- tfit[tfit$TITLE==temp_string,'Nz']
      tempser <- read.table(file=con,skip=1,header=F,nrows=nn)
      # seleziona la colonna delle date
      temp <- as.numeric(unlist(strsplit(as.character(tempser[,1]),split="-")))
      datainizio <- rev(as.numeric(unlist(strsplit(as.character(tempser[1,1]),split="-"))))
      tempser <- ts(tempser[,-1],start=datainizio,frequency=freq)

      residui <- tbind(residui,tempser[,4])
   }

   close(con)

   residui <- ifelse(residui==0,NA,residui)
   #  acquisisci gradi di liberta per test di Ljung-Box

   ordine <- rep(NA,length(finalnames))
   statistica <- rep(NA,length(finalnames))

   for (ii in 1:length(finalnames)){
      temp_number <- selected[ii]
      temp_string <- finalnames[ii]
      nomefile <- paste('^t',temp_number,'_.*out',sep="")
      nomefile <- paste(x,dir(x)[grep(nomefile,dir(x))],sep='/')
      con <- file(nomefile,"r",encoding="latin1")
      #   print(ii)
      counter <- 0

      tryCatch(
      repeat{
         string <- readLines(con=con,n=1)
         if (grepl('LJUNG-BOX',string)) break
         counter <- counter+1
      },error=function(e) cat("per la serie",finalnames[ii],"non c'e statistica di LB",'\n')
      )

      # separa gli elementi della riga dove c'e' la stringa LJUNG-BOX
      if (length(string)>0){
      temp <- unlist(strsplit(trim(string),'[[:space:]]+'))
      ordine[ii]  <-  as.numeric(temp[6])
      statistica[ii] <- as.numeric(temp[8])
      } else {}
      close(con)
   }


   if(length(finalnames)==1){
      names(serie_grezza) <- finalnames
      names(serie_sa) <- finalnames
      names(serie_trend) <- finalnames
      names(serie_stochsa) <- finalnames
      names(serie_stochtrend) <- finalnames
      names(serie_wd) <- finalnames
      names(serie_linear) <- finalnames
      names(serie_irreg) <- finalnames
      names(serie_totseas) <- finalnames
      names(serie_stochseas) <- finalnames
      names(serie_si) <- finalnames
      names(determ_eff) <- finalnames
      names(residui) <- finalnames
   } else {
      colnames(serie_grezza) <- finalnames
      colnames(serie_sa) <- finalnames
      colnames(serie_trend) <- finalnames
      colnames(serie_stochsa) <- finalnames
      colnames(serie_stochtrend) <- finalnames
      colnames(serie_wd) <- finalnames
      colnames(serie_linear) <- finalnames
      colnames(serie_irreg) <- finalnames
      colnames(serie_totseas) <- finalnames
      colnames(serie_stochseas) <- finalnames
      colnames(serie_si) <- finalnames
      colnames(determ_eff) <- finalnames
      colnames(residui) <- finalnames
   }

   if (freq==12){
      tfit$LBprob <- round(pchisq(q=ttest$Q.val,df=c(freq*2-tfit$P-tfit$Q-tfit$BP-tfit$BQ),lower.tail=FALSE),3)
      tfit$LBsqprob <- round(pchisq(q=ttest$Q2,df=c(freq*2),lower.tail=FALSE),3)
   } else if (freq==4) {
      tfit$LBprob <- round(pchisq(q=ttest$Q.val,df=c(freq*3-tfit$P-tfit$Q-tfit$BP-tfit$BQ),lower.tail=FALSE),3)
      tfit$LBsqprob <- round(pchisq(q=ttest$Q2,df=c(freq*3),lower.tail=FALSE),3)
   }



   # calcola correttamente i gdl dei test di autocorrelazione dei residui
   tfit$Q.val2 <- rep(0,length(tfit$LBprob))
   tfit$Q.ord2 <- rep(0,length(tfit$LBprob))
   tfit$LBprob2 <- rep(0,length(tfit$LBprob))
   tfit$LBsqprob2 <- rep(0,length(tfit$LBprob))
   tfit$Q.val2[selected] <- statistica
   tfit$Q.ord2[selected] <- ordine
   tfit$LBprob2[selected] <- round(pchisq(q=tfit$Q.val2[selected],df= c( ordine -tfit$P[selected] -tfit$Q[selected]- tfit$BP[selected]-tfit$BQ[selected]),lower.tail=FALSE),3)
   tfit$LBsqprob2[selected] <- round(pchisq(q=ttest$Q2[selected],df= ordine,lower.tail=FALSE),3)



   tfit$BPprob <- round(pchisq(q=TsignifS$QS,df=c(2),lower.tail=FALSE),3)
   tfit$NORMprob <- rep(0,length(ttest$N.test))
   tfit$NORMprob <- ifelse(is.na(as.double(ttest$N.test)),0,
                           round(pchisq(q=as.double(ttest$N.test),df=2,lower.tail=FALSE),3)
                           )

   tfit <- tfit[tfit[,"TITLE"] %in% finalnames,]



   temp <- merge(tfit,ttest,by='TITLE',sort=FALSE)
   temp <- merge(temp,TsignifS,by='TITLE',sort=FALSE)
   temp <- merge(temp,tarmapar,by='TITLE',sort=FALSE)
   temp <- merge(temp,sgeneral,by='TITLE',sort=FALSE)
   temp <- merge(temp,sparami,by='TITLE',sort=FALSE)
   temp <- merge(temp,sparamii,by='TITLE',sort=FALSE)
   temp <- merge(temp,sparami3,by='TITLE',sort=FALSE)
   diagmat <- temp[,c('TITLE','Nz','Lam','Mean','P','D','Q','BP','BD','BQ',
                      'TVP.m','TVP.m.t','TVP.phi1','TVP.phi1.t','TVP.phi2','TVP.phi2.t','TVP.phi3','TVP.phi3.t','TVP.bphi1','TVP.bphi1.t',
                      'TVP.th1','TVP.th1.t','TVP.th2','TVP.th2.t','TVP.th3','TVP.th3.t','TVP.bth1','TVP.bth1.t',
                      'SE.res.','BIC','Q.val','Q.ord2','LBprob2', 
                      'Q2','Q.ord2','LBsqprob2',
                      'N.test','NORMprob','SK.t.','KUR.t.','QS','BPprob','modchange','modapprox','checkacf','checkccf',
                      'sdinn_tc','sdinn_s','sdinn_trans','sdinn_u','sdinn_sa','seest_conc_tc','seest_conc_sa', 'stderr_rg_1_tc','stderr_rg_1_sa',
                    'conv_tc_1y','conv_sa_1y','conv_tc_5y','conv_sa_5y','sigseas_hist','sigseas_prel','sigseas_forc')]

   colnames(diagmat) <- c('TITLE','OBS','LOG','ORD.m','ORD.p','ORD.d','ORD.q','ORD.bp','ORD.bd','ORD.bq',
                          'TVP.m','TVP.m.t','TVP.phi1','TVP.phi1.t','TVP.phi2','TVP.phi2.t','TVP.phi3','TVP.phi3.t','TVP.bphi1','TVP.bphi1.t',
                          'TVP.th1','TVP.th1.t','TVP.th2','TVP.th2.t','TVP.th3','TVP.th3.t','TVP.bth1','TVP.bth1.t',
                          'SER','BIC','QR.val','QR.ord','QR.prob',
                       'QR2.val','QR2.ord','QR2.prob',
                       'N.test','N.prob','TVA','TVC','QSR.val','QSR.prob','MODC','MODA','ACF','CCF',
                       'SEI.tc','SEI.s','SEI.trans','SEI.u','SEI.sa','SES.tc','SES.sa',
                       'SETCC.tc','SETCC.sa',
                       'CONV.1ytc','CONV.1ysa','CONV.5ytc','CONV.5ysa','SIGNS.hist','SIGNS.prel','SIGNS.forc')

   out <- NULL

   out$temp <- temp
   out$diagmat <- diagmat
   out$serie_grezza <- serie_grezza
   out$serie_sa <- serie_sa
   out$serie_trend <- serie_trend
   out$serie_stochsa <- serie_stochsa
   out$serie_stochtrend <- serie_stochtrend
   out$serie_wd <- serie_wd
   out$serie_linear <- serie_linear
   out$serie_irreg <- serie_irreg
   out$serie_totseas <- serie_totseas
   out$determ_eff <- determ_eff
   out$residui <- residui
   out$tfit <- tfit
   out$nomi <- finalnames
   out$sgeneral <- sgeneral
   out$sparami <- sparami
   out$sparamii <- sparamii
   out$tarmapar <- tarmapar
   #out$tdeterm <- tdeterm
   out$serie_stochseas <- serie_stochseas
   out$serie_si <- serie_si

   class(out) <- "output_ts"

   out
}

plot.output_tsplus <- function(x,i,...){
   if (length(x$nomi)>1){
      oldpar <- par(no.readonly = T)
      require(tframe)
      nomi <- x$nomi
      newx <- x[c("serie_grezza","serie_sa","serie_wd","serie_linear","serie_totseas","determ_eff","serie_irreg","serie_stochseas","serie_si")]
      xvalid <- lapply(newx,function(x,i) trimNA(x[,i]),i)

      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      plot(xvalid$serie_grezza,main='grezza e corretta',ylab='',xlab='',las=1);lines(xvalid$serie_wd,col=2)
      plot(xvalid$serie_grezza,main='grezza e destagionalizzata',ylab='',xlab='',las=1);lines(xvalid$serie_sa,col=2)
      plot(xvalid$serie_totseas,main='comp. stagionale',ylab='',xlab='',las=1)
      #   plot(dati[,tempname],main='serie aggiustata',ylab='',xlab='',las=1);lines(serie_linear,col=2)
      if(diff(range( 
                    xvalid$determ_eff[!is.na(xvalid$determ_eff)]
                    ))<1){
         #plot(xvalid$determ_eff,main='effetti deterministici',ylim=range(xvalid$determ_eff)+c(-1,1),ylab='',xlab='',las=1)
         plot(xvalid$determ_eff,main='effetti deterministici',ylab='',xlab='',las=1)
      } else {
         plot(xvalid$determ_eff,main='effetti deterministici',ylab='',xlab='',las=1)
      }
      mtext(line=0,nomi[i],outer=T,font=2,cex=1.2)
      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      {
         monthplot(xvalid$serie_si,type='h',lwd.base=0,main="SI ratio")
         monthplot(xvalid$serie_stochseas,type='l',lwd.base=0,lwd=2,add=TRUE,col=2)
      }
      spectrum(cbind(xvalid$serie_grezza,xvalid$serie_sa),col=c(1,2),spans=c(3,3),main='periodogramma')
      legend(x='topright',legend=c('serie grezza','serie destagionalizzata'),lty=c(1,1),col=c(1,2),inset=0.01)
      mtext(line=0,nomi[i],outer=T,font=2,cex=1.2)
      par <- oldpar
   } else {
      oldpar <- par(no.readonly = T)
      require(tframe)
      nomi <- x$nomi
      newx <- x[c("serie_grezza","serie_sa","serie_wd","serie_linear","serie_totseas","determ_eff","serie_irreg","serie_stochseas","serie_si")]
      xvalid <- lapply(newx,function(x) trimNA(x))

      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      plot(xvalid$serie_grezza,main='grezza e corretta',ylab='',xlab='',las=1);lines(xvalid$serie_wd,col=2)
      plot(xvalid$serie_grezza,main='grezza e destagionalizzata',ylab='',xlab='',las=1);lines(xvalid$serie_sa,col=2)
      plot(xvalid$serie_totseas,main='comp. stagionale',ylab='',xlab='',las=1)
      #   plot(dati[,tempname],main='serie aggiustata',ylab='',xlab='',las=1);lines(serie_linear,col=2)
      if(diff(range( 
                    xvalid$determ_eff[!is.na(xvalid$determ_eff)]
                    ))<1){
         #plot(xvalid$determ_eff,main='effetti deterministici',ylim=range(xvalid$determ_eff)+c(-1,1),ylab='',xlab='',las=1)
         plot(xvalid$determ_eff,main='effetti deterministici',ylab='',xlab='',las=1)
      } else {
         plot(xvalid$determ_eff,main='effetti deterministici',ylab='',xlab='',las=1)
      }
      mtext(line=0,nomi[i],outer=T,font=2,cex=1.2)
      par(las=1,mfrow=c(2,2),mar=c(2,2,2,2),oma=c(3,3,3,3),xpd=F)
      {
         monthplot(xvalid$serie_si,type='h',lwd.base=0,main="SI ratio")
         monthplot(xvalid$serie_stochseas,type='l',lwd.base=0,lwd=2,add=TRUE,col=2)
      }
      spectrum(cbind(xvalid$serie_grezza,xvalid$serie_sa),col=c(1,2),spans=c(3,3),main='periodogramma')
      legend(x='topright',legend=c('serie grezza','serie destagionalizzata'),lty=c(1,1),col=c(1,2),inset=0.01)
      mtext(line=0,nomi[i],outer=T,font=2,cex=1.2)
      par <- oldpar
   }
}
