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



plot.output_ts <- function(x, i, old=NA, quarterly=FALSE, last_percentage_vars=NA, ...){
  #library(gridExtra)
  library(cowplot)
  
  if (length(x$nomi)>1){
    #browser()
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
    
    
    if(is.list(old) && length(old$serie_sa)>0) # se old è NA (quindi sarà valorizzato come lista) non eseguire il codice sotto
    {  
      oldx <- old[c("serie_grezza","serie_sa","serie_wd","serie_linear","serie_totseas","determ_eff","serie_irreg","serie_stochseas","serie_si")]

      xvalid_old <- lapply(oldx,function(x,i) trimNA(x[,i]),i) 
      
      if(!is.na(last_percentage_vars))
      {
        last <- last_percentage_vars
      }  
      else 
      {  
        if(quarterly){
          last=8
        }else{
          last=24
        }
      }
      
      grafico_sa <- percentage_variations_plot(xvalid_old$serie_sa, xvalid$serie_sa, last=last, quarterly=quarterly)
      grafico_wd <- percentage_variations_plot(xvalid_old$serie_wd, xvalid$serie_wd, last=last, quarterly=quarterly)
      
      
      grafico_sa <- grafico_sa +
        labs(title = paste("Confronto tra variazioni congiunturali su dati sa - ultime", last, "osservazioni"), x = "", y = "%") +
        theme(plot.title = element_text(size = 10, hjust = 0.5))  # Imposta la dimensione del testo del titolo a 9 e centralo
      
      
      grafico_wd <- grafico_wd +
        labs(title = paste("Confronto tra variazioni congiunturali su dati wd - ultime", last, "osservazioni"), x = "", y = "%") +
        theme(plot.title = element_text(size = 9, hjust = 0.5))  # Imposta la dimensione del testo del titolo a 9 e centralo
      
      # library(gridExtra)
      # grid_arrange <- grid.arrange(grafico_sa, grafico_wd, ncol = 2)
      # 
      # print(grid_arrange)
      
      # library(cowplot)
      # grid <- plot_grid(grafico_sa, grafico_wd, ncol = 2, align = "h", axis = "tb")
      # 
      # print(grid)
      # mtext(line=0,nomi[i],outer=T,font=2,cex=1.2, adj = 0.5, padj = 0.5) # metto un'etichetta con il mome della serie in cima alla pagina
      
      grid_plot <- cowplot::plot_grid(grafico_sa, grafico_wd, ncol = 2, align = "h", axis = "tb")
      
      # Crea l'etichetta con il nome della serie
      label <- cowplot::ggdraw() +
        draw_label(nomi[i], x = 0.5, y = 1, vjust = 1.5, hjust = 0.5, size = 14, fontface = "bold")
      
      # Combina la griglia dei grafici con l'etichetta
      grid_plot_with_label <- cowplot::plot_grid(label, grid_plot, ncol = 1, rel_heights = c(1, 10))
      print(grid_plot_with_label)
      
      
      # Se non voglio i grafici affiancati, commento la griglia e plotto singolarmente
      #plot(grafico_sa)
      #plot(grafico_wd)
      
      
      
    }
    
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
  
  out$TsignifS <- TsignifS # added by me
  
  
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

compute_percentage_variations <- function(serie_old, serie_new) # TODO in realtà a sua volta è divisibile in due chiamate, una per old e una per new
{
  var_cong_old <- ts(numeric(length(serie_old)), frequency = frequency(serie_old), start = start(serie_old))
  var_cong_new <- ts(numeric(length(serie_new)), frequency = frequency(serie_new), start = start(serie_new))
  
  var_cong_old[1] = NA
  var_cong_new[1] = NA
  for (j in 2:length(serie_old))
  {
    var_cong_old[j] <- 100*(serie_old[j] - serie_old[j-1]) / serie_old[j-1]
    var_cong_new[j] <- 100*(serie_new[j] - serie_new[j-1]) / serie_new[j-1]
  }
  
  return(list(var_cong_old,var_cong_new))
}



percentage_variations_plot <- function(serie_old, serie_new, last=24, quarterly=FALSE)
{
  percentage_variations_list <- compute_percentage_variations(serie_old, serie_new)
  var_cong_old               <- percentage_variations_list[[1]]
  var_cong_new               <- percentage_variations_list[[2]]
  
  # Carica le librerie necessarie
  library(ggplot2)
  library(lubridate)
  
  # Seleziona solo gli ultimi "last" valori delle serie storiche
  var_cong_old <- tail(var_cong_old, last)
  var_cong_new <- tail(var_cong_new, last)
  
  
  library(zoo)
  anno_mese_old <- tail(format(as.Date(as.yearmon(time(serie_old))), "%Y-%m"),last)
  anno_mese_new <- tail(format(as.Date(as.yearmon(time(serie_new))), "%Y-%m"),last)
  
  
  
  # Crea un dataframe per var_cong_old
  dati_var_cong_old <- data.frame(
    Mesi_o_Trim = anno_mese_old,
    Valore = as.numeric(var_cong_old),
    Tipo = "old model"
  )
  
  
  # Crea un dataframe per var_cong_new
  dati_var_cong_new <- data.frame(
    Mesi_o_Trim = anno_mese_new,
    Valore = as.numeric(var_cong_new),
    Tipo = "new model"
  )
  
  # Unisci i dataframe
  dati_affiancati <- rbind(dati_var_cong_old, dati_var_cong_new)
  
  # Calcola l'origine dinamica: data dell'ultimo valore meno "last" mesi
  data_fine <- tail(dati_affiancati$Mesi_o_Trim, 1)
  origin <- as.Date(as.yearmon(data_fine)) %m-% months(last)  # Sottrai "last" mesi
  
  if(quarterly)
  {
    # Sostituisci i mesi con i quarti nel vettore
    dati_affiancati$Mesi_o_Trim <- gsub("\\b01\\b", "q1", dati_affiancati$Mesi_o_Trim) # \\b per non sostituire lo 01 di 2018
    dati_affiancati$Mesi_o_Trim <- gsub("04", "q2", dati_affiancati$Mesi_o_Trim)
    dati_affiancati$Mesi_o_Trim <- gsub("07", "q3", dati_affiancati$Mesi_o_Trim)
    dati_affiancati$Mesi_o_Trim <- gsub("10", "q4", dati_affiancati$Mesi_o_Trim)
  }  
  
  
  # Crea il grafico per dati con ggplot
  grafico <- ggplot(dati_affiancati, aes(x = Mesi_o_Trim, y = Valore, fill = Tipo)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.4) +
    labs(x = "Mese e Anno", y = "Valore", title = "Confronto tra var_cong_sa_old e var_cong_sa_new") +
    labs(fill = NULL) +  # Rimuovi l'etichetta del fill dalla legenda
    theme_minimal() +
    scale_x_discrete(labels = dati_affiancati$Mesi_o_Trim) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          legend.position = "top",          # Sposta la legenda in alto
          legend.justification = "center",  # Ancora la legenda al centro
          legend.box = "horizontal")        # Mostra la legenda come una barra orizzontale
  
  
  
  
  
  # Visualizza il grafico
  return(grafico)
  
}


make_files_to_send_folder <- function(folder_name = "toSend", to_send, begin_cleaning_folder = FALSE) {
  # Controlla se la cartella folder_name esiste, altrimenti creala
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  }
  
  # Pulisce la cartella se begin_cleaning_folder è TRUE
  if (begin_cleaning_folder) {
    files_to_remove <- list.files(folder_name, full.names = TRUE)
    file.remove(files_to_remove)
  }
  
  # Cicla attraverso i file da copiare
  for (file_path in to_send) {
    # Ottieni il nome del file dal percorso
    file_name <- basename(file_path)
    
    # Copia il file nella cartella toSend
    file.copy(file_path, file.path(folder_name, file_name), overwrite = TRUE)
  }
}


prepare_JDplus_file <- function(input_file, output_file){
  # leggi il contenuto del file di input
  
  
  input_text <- readLines(input_file)
  
  # crea la cartella "regr" se non esiste gi?
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



synchronize_models <- function(vecchi, nuovi) {
  
  elementi_mancanti_nuovi <- setdiff(vecchi$nomi, nuovi$nomi)
  indici_da_eliminare_nuovi <- which(vecchi$nomi %in% elementi_mancanti_nuovi)
  
  elementi_mancanti_vecchi    <- setdiff(nuovi$nomi, vecchi$nomi)
  indici_da_aggiungere_vecchi <- which(nuovi$nomi %in% elementi_mancanti_vecchi)
  
  
  
  # Funzione per applicare il filtraggio alle matrici
  filtraggio_matrice <- function(matrice, nome, indici_da_eliminare_nuovi, indici_da_aggiungere_vecchi) {
    #browser()
    # gestione modelli chiusi
    if(length(indici_da_eliminare_nuovi)>0)
    {  
      matrice_filtrata <- matrice[, -indici_da_eliminare_nuovi, drop = FALSE]
    }else{ matrice_filtrata<- matrice}
    
    
    # gestione modelli aggiunti
    col_da_aggiungere <- nuovi[[nome]][, indici_da_aggiungere_vecchi]
    
    # Aggiungi le colonne da aggiungere nelle posizioni specificate
    for (i in seq_along(indici_da_aggiungere_vecchi)) {
      indice <- indici_da_aggiungere_vecchi[i]
      
      # Inserisci la nuova colonna nella posizione desiderata
      
      # Estrai le colonne dalla matrice e da col_da_aggiungere
      if(indice < ncol(matrice_filtrata)) # colonne da aggiungere in mezzo alla matrice
      {
        colonne_esistenti <- matrice_filtrata[, 1:(indice - 1), drop = FALSE]
        colonna_da_aggiungere_corrente <- col_da_aggiungere[,i]
        colonne_rimanenti <- matrice_filtrata[, indice:(ncol(matrice_filtrata)), drop = FALSE]
        matrice_filtrata <- cbind(colonne_esistenti, colonna_da_aggiungere_corrente, colonne_rimanenti)
        
      } else{ # colonne da aggiungere alla fine
        colonne_esistenti <- matrice_filtrata[, 1:(indice - 1), drop = FALSE]
        colonna_da_aggiungere_corrente <- col_da_aggiungere[,i]
        matrice_filtrata <- cbind(colonne_esistenti, colonna_da_aggiungere_corrente)
      }
      
      # Unisci le colonne
      
    }
    
    return(matrice_filtrata)
  }
  
  # Funzione per applicare il filtraggio ai dataframe
  filtraggio_df <- function(df, nome) {
    # in caso di modelli chiusi
    df_filtrato <- df[df$TITLE %in% nuovi$nomi, , drop = FALSE]
    
    # In caso di modelli aggiunti in nuovi, che non ci sono in vecchi, per semplicità li pongo in vecchi uguali a nuovi
    
    elementi_mancanti_vecchi    <- setdiff(nuovi$nomi, vecchi$nomi)
    
    nuovo_df                   <- nuovi[[nome]]
    righe_da_aggiungere_vecchi <- nuovo_df[nuovo_df$TITLE %in% elementi_mancanti_vecchi, , drop = FALSE]
    
    df_filtrato <- rbind(df_filtrato, righe_da_aggiungere_vecchi)
    
    return(df_filtrato)
  }
  
  # Applica la funzione di filtraggio a ciascun elemento nella lista
  #browser()
  vecchi_filtrati <- mapply(function(elem, nome) {
    if (is.matrix(elem)) {
      filtraggio_matrice(elem, nome, indici_da_eliminare_nuovi, indici_da_aggiungere_vecchi)
    } else if (is.data.frame(elem)) {
      filtraggio_df(elem, nome)  # Passa il nome al filtraggio_df
    } else {
      return(elem)  # Mantieni gli oggetti che non sono matrici o dataframe inalterati
    }
  }, vecchi, names(vecchi), SIMPLIFY = FALSE)
  vecchi_filtrati$nomi <- nuovi$nomi
  
  
  return(vecchi_filtrati) 
}


# Funzione per copiare tutti i file dalla cartella al livello superiore
copia_files_al_livello_superiore <- function(percorso) {
  # Ottieni la lista dei file nella cartella specificata dal percorso
  lista_file <- list.files(path = percorso, full.names = TRUE)
  
  # Copia i file al livello superiore
  for (file in lista_file) {
    nuovo_percorso <- file.path(dirname(percorso), basename(file))
    
    file.copy(file, nuovo_percorso)
  }
}

# Funzione per rinominare i file in un certo percorso aggiungendo un suffisso
aggiungi_suffisso_copia_liv_superiore <- function(percorso_temp = "temp", suffisso_gia_presente, suffisso) {
  
  # Rinomina i file aggiungendo il suffisso
  lista_file <- list.files(path = percorso_temp, full.names = TRUE)
  for (file in lista_file) {
    # Estrai il nome del file e l'estensione
    nome_file <- basename(file)
    estensione <- tools::file_ext(nome_file)
    
    # Rimuovi l'estensione dal nome del file
    nome_base <- tools::file_path_sans_ext(nome_file)
    
    # Verifica se il suffisso_gia_presente è presente nel nome del file
    if (!grepl(suffisso_gia_presente, nome_base)) {
      # Aggiungi il suffisso prima dell'estensione
      nuovo_nome <- paste0(nome_base, "_", suffisso, ".", estensione)
      
      # Rinomina il file
      file.rename(file, file.path(percorso_temp, nuovo_nome))
    }
  }
  
  # Copia i file al livello superiore rinominandoli se necessario
  copia_files_al_livello_superiore(percorso_temp)
  
  # Rimuovi la cartella originale e il suo contenuto
  unlink(percorso_temp, recursive = TRUE)
}



