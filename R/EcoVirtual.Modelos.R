####################################################
#### Dialogs boxes for RcmdrPlugin.EcoVirtual package
### Alexandre Adalardo de Oliveira 17 fevereiro 2010
### version: JAN 2016
####################################################
## por padrao .First.lib nao existe mais
# .First.lib <- function(libname, pkgname){
#     if (!interactive()) return()
#     Rcmdr <- options()$Rcmdr
#     plugins <- Rcmdr$plugins
#     if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
#         Rcmdr$plugins <- c(plugins, pkgname)
#         options(Rcmdr=Rcmdr)
#         closeCommander(ask=FALSE, ask.save=TRUE)
#         Commander()
#         }
#     }
####################################################
## usando a funcao do RcmdrPlugin.TeachingDemo (Jonh Fox)
# Note: the following function (with contributions from Richard Heiberger and Milan Bouchet-Valat)# can be included in any Rcmdr plug-in package to cause the package to load
# the Rcmdr if it is not already loaded
.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())    
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}
########################################################
modelo1 <- function(N0,lamb,tmax, intt= 1) 
{
  ## logical tests for initial conditions
  #   N0 <- round(as.numeric(tclvalue(noVar)))
  if (is.na(N0) || N0 <= 0) 
  {
    stop("Number of individuals at the simulation start must be a positive integer")
    #            return()
  }
  #       tmax <- round(as.numeric(tclvalue(tmaxVar)))
  if (is.na(tmax) || tmax <= 0) 
  {
    stop("Number of simulations must be a positive integer")
    #            return()
  }
  ##########################################
  #st<-0:tmax
  ntseq<-seq(0,tmax,by=intt) 
  resulta <- matrix(NA,nrow=length(ntseq), ncol=3)
  nc<-length(ntseq) -1
  rexp0=log(lamb)
  radj=rexp0*intt
  ladj=exp(radj)
  resulta[,1]<-ntseq
  resulta[,2]<-N0*exp(radj*(0:nc))
  resulta[,3]<-N0*ladj^(0:nc)
  ntmax=N0*lamb^tmax
  if(N0 <= ntmax)
  {
    ymax<-ntmax
    ymin<-N0
  }else
  {
    ymax<-N0
    ymin<-ntmax
  }
  plot(seq(0,tmax, len=10), seq(ymin,ymax,len=10), type="n", main="Modelo 1", xlab="Time", ylab="Population Size (N)", cex.axis=1.3, cex.lab=1.3, xlim=c(0,tmax), ylim=c(ymin, ymax), bty="n")

  ##segments(x0=resulta[- dim(resulta)[1],1], y0=resulta[- dim(resulta)[1],3], x1=resulta[- 1,1], y1=resulta[- dim(resulta)[1],3], lty=2, col="blue")
  ##segments(x0=resulta[- 1,1], y0=resulta[- dim(resulta)[1],3], x1=resulta[- 1,1], y1=resulta[- 1,3], lty=2, col="blue")
  seqt=seq(0,tmax,len=1000)
  radj02<-rexp0*tmax/1000
  points(seqt, N0*exp(rexp0*seqt), type="l", lwd=2)
  points(resulta[,1], resulta[,3],pch=16, col="blue")
  invisible(resulta)
}
popExpDb<-function()
{
  dialogName<-"popExpDb" ### inserido
  def <- list(noVar=10, lambVar= .5,tmaxVar=10,inttVar=0.2) # inserido
  initial <- getDialog(dialogName, defaults= def)
  initializeDialog(title = gettextRcmdr("Modelo 1"))
  noVar <- tclVar(initial$noVar)
  noEntry <- tkentry(top, width = "4", textvariable = noVar)
  lambVar <- tclVar(initial$lambVar)
  tmaxVar <- tclVar(initial$tmaxVar)
  tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
  inttVar<-tclVar(initial$inttVar)

  lambEntry<-tkscale(top, from=-1, to=1, showvalue=TRUE, variable=lambVar, resolution=0.01, orient="horizontal") #, command=set.gr)
  inttEntry <- tkscale(top, from=0.01, to=1, showvalue=TRUE, variable=inttVar, resolution=0.01, orient="horizontal")#,command=set.gr)

  onOK <- function() 
  {
    closeDialog()
    command <- paste("modelo1(N0 = ", as.numeric(tclvalue(noVar)), ", lamb = ", exp(as.numeric(tclvalue(lambVar))), ", tmax = ", as.numeric(tclvalue(tmaxVar)),", intt = ", as.numeric(tclvalue(inttVar)),")", sep="")  
    justDoIt("dev.new()")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, values = list(noVar=as.numeric(tclvalue(noVar)), lambVar=as.numeric(tclvalue(lambVar)), tmaxVar=as.numeric(tclvalue(tmaxVar)), inttVar=as.numeric(tclvalue(inttVar))), resettable = FALSE) ## inserido
  }

  OKCancelHelp(helpSubject = "modelo1", reset=dialogName, apply=dialogName) # modificado
  tkgrid(tklabel(top, text = "Tempo Máximo"), tmaxEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Intervalo de tempo "), inttEntry, sticky = "e")
  tkgrid(tklabel(top, text="Parâmetros :", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "Tamanho populacional inicial (N₀) "), noEntry, sticky = "e")
  tkgrid(tklabel(top, text = "r  "), lambEntry, sticky = "e")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  tkgrid.configure(tmaxEntry, sticky = "w")
  tkgrid.configure(inttEntry, sticky = "w")
  tkgrid.configure(noEntry, sticky = "w")
  tkgrid.configure(lambEntry, sticky = "w")
  dialogSuffix(rows = 6, columns = 2, focus = tmaxEntry)
}
############################################################
model2<-function(N0, tmax, r, K, ext=FALSE)
{
  resulta=matrix(NA, nrow=tmax+1,ncol=3)
  colnames(resulta)=c("time", "Continuous Model ", "Discrete Model")
  resulta[,1]=0:tmax
  resulta[1,3]=N0
  #####################
  if (is.na(N0) || N0 <= 0) 
  {
    stop(message = "Number of individuals at the simulation start must be a positive integer")
  }
  if (is.na(tmax) || tmax <= 0) 
  {
    stop("Number of simulations must be a positive integer")
  }
  if (is.na(K) || K <= 0)
  {
    stop("Carrying Capacity (K) must be a positive integer")
  }
  ######### Ajuste do rdiscreto ############
  #lamb=exp(r)
  #rd=lamb-1
  resulta[,2]<-K/(1+((K-N0)/N0)*exp(-r*(0:tmax)))
  ##########################################
  for(t in 2:(tmax+1))
  {
    #ifelse(nCont<0,resulta[t,2]<-0, resulta[t,2]<-nCont)
    lastN=resulta[t-1,3]
    nDisc<-lastN+r*lastN*(1-lastN/K)
    resulta[t,3]<-nDisc
    if(ext==TRUE & nDisc<0)
    {
      resulta[t,3]<-0
    }
  }
  rangN<-range(resulta[,c(2,3)], na.rm=TRUE)
  if(rangN[1]==-Inf){rangN[1]=-10}
  if(rangN[2]==Inf){rangN[2]=K*1.2}
  plot(resulta[,1], seq(floor(rangN[1]), ceiling(max(rangN[2],K)), len=dim(resulta)[1]), type="n", xlab="Time (t)", main="Modelo 2", ylab="Population size (N)",cex.lab=1.3, cex.axis=1.3, cex.main=1.5, ylim=c(rangN[1], rangN[2]+5), bty="n")
  polygon(c(-10,-10, tmax*1.2, tmax*1.2), c(-40,0,0,-40), col="gray80")
  ###########################
  ### continuous logistical #
  ###########################
  seqt=seq(0,tmax,len=1000)
  #radj0<-r*tmax/1000
  seqN<-K/(1+((K-N0)/N0)*exp(-r*(seqt)))
  points(seqt, seqN, type="l", lwd=2)
  #lines(resulta[,1],resulta[,3], col="red", lwd=2, lty=4)	
  #legend("bottomright", colnames(resulta)[2:3],lty=c(1,4),col=c(1,2),bty="n", lwd=2)
  abline(h=K, lty=3, col="blue", lwd=2)
  abline(h=0)
  #text(x=0.2, y=K+1, "Carrying capacity", col="blue",adj=c(0,0), cex=0.7)
  #text(x=tmax*0.4, y= resulta[(tmax/2),2], paste("r=", r),pos=3)
  #title(sub=paste("rd = r = ", round(r,3)),cex.sub=0.9)
  invisible(resulta)
}
#popLog(N0=10, r=0.05, K=80, tmax=100, ext=FALSE)
popLogDb<-function () 
{
  dialogName<-"popLogDb" 
  def <- list(noVar=10, rVar= 0.5, kVar=100, tmaxVar=30, extVar=0)
  initial <- getDialog(dialogName, defaults= def)
  initializeDialog(title = gettextRcmdr("Modelo 2"))

  noVar <- tclVar(initial$noVar)
  rVar <- tclVar(initial$rVar)
  kVar <- tclVar(initial$kVar)
  tmaxVar <- tclVar(initial$tmaxVar)
  noEntry <- tkentry(top, width = "4", textvariable = noVar)
  tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
  kEntry<-tkentry(top, width = "4", textvariable = kVar)
  extVar <- tclVar(initial$extVar)
  extBox <- tkcheckbutton(top, variable = extVar)
  #################################################################
  # funcao da atualizacao pelo barra de rolagem
  ## set.gr=function(...)
  ## 	{
  ## 	command <- paste("popLog(N0= ", as.numeric(tclvalue(noVar)), ", r = ", as.numeric(tclvalue(rVar)),", K = ", as.numeric(tclvalue(kVar)),", tmax =", round(as.numeric(tclvalue(tmaxVar))), ", ext =", as.logical(as.numeric(tclvalue(extVar))),")", sep = "")
  ## 	doItAndPrint(command)
  ## #	tkfocus(CommanderWindow())
  ## 	}
  ##############################################################
  rEntry <-tkscale(top, from=-1, to=1, showvalue=TRUE, variable=rVar, resolution=0.01, orient="horizontal")
  #, command=set.gr)
  #kEntry <- tkscale(top, from=0, to= 10*as.numeric(tclvalue(noVar)), showvalue=TRUE, variable=kVar, resolution=round(0.1*as.numeric(tclvalue(noVar))), orient="horizontal", command=set.gr)
  ##############################################################
  onOK <- function() 
  {
    closeDialog()            
    N0 <- round(as.numeric(tclvalue(noVar)))
    #kEntry <- tkscale(top, from=0, to= 10*N0, showvalue=TRUE, variable=kVar, resolution=round(0.1*N0), orient="horizontal")
    tmax <- round(as.numeric(tclvalue(tmaxVar)))
    K <- as.numeric(tclvalue(kVar))
    r=as.numeric(tclvalue(rVar))

    command <- paste("dev.new(); model2(N0= ",N0, ", r = ", r,", K = ", K,", tmax =", tmax, ", ext = FALSE)", sep = "")
    putDialog(dialogName, values = list(noVar=as.numeric(tclvalue(noVar)), rVar= as.numeric(tclvalue(rVar)), kVar= as.numeric(tclvalue(kVar)) , tmaxVar=as.numeric(tclvalue(tmaxVar)), extVar=as.logical(as.numeric(tclvalue(extVar)))), resettable = FALSE)
    if(as.logical(as.numeric(tclvalue(extVar))))
      justDoIt(paste('dev.new();curve(',r,'*x*(1 - x/',K,'), xlim=c(0, ',K,'), xlab="N", ylab="dN/dt", bty="l", main="Modelo 2")', sep=""))
    else
      doItAndPrint(command)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "modelo2", reset=dialogName, apply=dialogName)
  tkgrid(tklabel(top, text = "Tempo máximo "), tmaxEntry, sticky = "e")
  tkgrid(tklabel(top, text="Parâmetros: ", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "Tamanho populacional inicial (N₀) "), noEntry, sticky = "e")
  tkgrid(tklabel(top, text = "K "), kEntry, sticky = "e")
  tkgrid(tklabel(top, text = "r "), rEntry, sticky = "se")
  tkgrid(tklabel(top, text = "Gráfico derivada versus N"), extBox, sticky = "e")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  tkgrid.configure(tmaxEntry, sticky = "w")
  tkgrid.configure(noEntry, sticky = "w")
  tkgrid.configure(kEntry, sticky = "w")
  tkgrid.configure(rEntry, sticky = "w")
  tkgrid.configure(extBox, sticky = "w")
  dialogSuffix(rows = 6, columns = 2, focus = tmaxEntry)
} 
##########################################
##########################################
model3=function(n01,n02,tmax,r1,r2,k1,k2,alfa,beta,df)
{
  resulta=matrix(0, ncol=3, nrow=tmax, dimnames=list(NULL, c("time", "Nsp1","Nsp2")))
  resulta[,1]=0:(tmax-1)
  resulta[1,c(2,3)]=c(n01,n02)
  for(t in 2:tmax)
  {
    nsp1=resulta[(t-1),2]
    nsp2=resulta[(t-1),3]
    resulta[t,2]=nsp1 + r1*nsp1*((k1-nsp1-alfa*nsp2)/k1)
    resulta[t,3]=nsp2 + r2*nsp2*((k2-nsp2-beta*nsp1)/k2)
    if (resulta[t,2]<1)  
    {
      resulta[t,2]=0
    }
    if (resulta[t,3]<1)  
    {
      resulta[t,3]=0
    }
  }
  #dev.new()
  #old=par(mfrow=c(1,2), mar=c(4,4,2,1))
  if(!df){
    plot(resulta[,1],resulta[,2],ylim=c(0,max(na.omit(resulta[,2:3]))),type="l",lty=4,xlab="time (t)",ylab="Population size", main="Population Growth", col="blue", lwd=1.5 )
    legend("topleft", legend=c("Sp. 1", "Sp. 2"), lty=4, col=c("blue", "green"), bty="n", cex=0.8)
    lines(resulta[,1],resulta[,3], col="green", lty=4, lwd=1.5)
  }
  else{
    plot(resulta[,2],resulta[,3],type="l",col="red",xlab="N1",ylab="N2",ylim=c(0,max(c(na.omit(resulta[,3]),k1/alfa,k2))),xlim=c(0,max(c(na.omit(resulta[,2]),k2/beta,k1))), man="Diagrama de Fases")
    points(resulta[length(resulta[,2]),2], resulta[length(resulta[,3]),3])
    legend("topright",legend=c("Populations trajectory"), lty=c(4,4,1), col=c("red"), bty="n", cex=0.8)
  }
  invisible(resulta)
}

compDb<-function () 
{
  dialogName<-"compDb" ### 
  def <- list(n01Var=10, n02Var=10, r1Var= 0.5, r2Var= 0.5, k1Var= 100, k2Var= 100, alfaVar= 1.2, betaVar= 0.8, tmaxVar=30, extVar=0) # lista de argumentos padrao
  initial <- getDialog(dialogName, defaults= def)
  initializeDialog(title = gettextRcmdr("Modelo 3"))
  n01Var <- tclVar(initial$n01Var)
  n01Entry <- tkentry(top, width = "4", textvariable = n01Var)
  n02Var <- tclVar(initial$n02Var)
  n02Entry <- tkentry(top, width = "4", textvariable = n02Var)
  r1Var <- tclVar(initial$r1Var)
  r1Entry=tkscale(top, from=-1, to=1, showvalue=TRUE, variable=r1Var, resolution=0.01, orient="horizontal")
  r2Var <- tclVar(initial$r2Var)
  r2Entry=tkscale(top, from=-1, to=1, showvalue=TRUE, variable=r2Var, resolution=0.01, orient="horizontal")
  k1Var <- tclVar(initial$k1Var)
  k1Entry <- tkentry(top, width = "4", textvariable = k1Var)
  k2Var <- tclVar(initial$k2Var)
  k2Entry <- tkentry(top, width = "4", textvariable = k2Var)
  alfaVar <- tclVar(initial$alfa)
  alfaEntry <- tkentry(top, width = "6", textvariable = alfaVar)
  betaVar <- tclVar(initial$beta)
  betaEntry <- tkentry(top, width = "6", textvariable = betaVar)
  tmaxVar <- tclVar(initial$tmaxVar)
  tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
  extVar <- tclVar(initial$extVar)
  extBox <- tkcheckbutton(top, variable = extVar)
  onOK <- function() 
  {
    closeDialog()
    n01 <- round(as.numeric(tclvalue(n01Var)))
    if (is.na(n01) || n01 < 0) 
    {
      errorCondition(message = "Number of individuals at the simulation start can't be negative")
      return()
    }
    n02 <- round(as.numeric(tclvalue(n02Var)))
    if (is.na(n02) || n02 < 0) 
    {
      errorCondition(message = "Number of individuals at the simulation start can't be negative")
      return()
    }
    tmax <- round(as.numeric(tclvalue(tmaxVar)))
    if (is.na(tmax) || tmax <= 0) 
    {
      errorCondition("Number of simulations must be a positive integer")
      return()
    }
    k1 <- as.numeric(tclvalue(k1Var))
    if (is.na(k1) || k1 <= 0)
    {
      errorCondition(message = "K₁ must be a positive integer")
      return()
    }
    k2 <- as.numeric(tclvalue(k2Var))
    if (is.na(k2) || k2 <= 0)
    {
      errorCondition(message = "K₁ must be a positive integer")
      return()
    }
    r1=as.numeric(tclvalue(r1Var))
    r2=as.numeric(tclvalue(r2Var))
    alfa=as.numeric(tclvalue(alfaVar))
    beta=as.numeric(tclvalue(betaVar))
    command <- paste("model3(n01= ",n01, ",n02= ",n02, ", r1 = ", r1,", r2 = ", r2,", k1 = ", k1,", k2 = ", k2,", alfa = ", alfa,", beta = ", beta,", tmax =", tmax, ", df=",as.logical(as.numeric(tclvalue(extVar))), ")", sep = "")
    justDoIt("dev.new()")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, values = list(n01Var=round(as.numeric(tclvalue(n01Var))), n02Var=round(as.numeric(tclvalue(n02Var))), r1Var= as.numeric(tclvalue(r1Var)), r2Var= as.numeric(tclvalue(r2Var)), k1Var= as.numeric(tclvalue(k1Var)), k2Var= as.numeric(tclvalue(k2Var)), alfaVar= as.numeric(tclvalue(alfaVar)), betaVar= as.numeric(tclvalue(betaVar)), tmaxVar=as.numeric(tclvalue(tmaxVar)), extVar=as.logical(as.numeric(tclvalue(extVar))) ), resettable = FALSE)
  }
  OKCancelHelp(helpSubject = "modelo3", reset=dialogName, apply=dialogName)
  tkgrid(tklabel(top, text = "Tempo máximo   "), tmaxEntry, sticky = "e")
  tkgrid(tklabel(top, text="Parâmetros da espécie 1 : ", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "População inicial (N₀₁)  "), n01Entry, sticky = "e")
  tkgrid(tklabel(top, text = "K₁  "), k1Entry, sticky = "e")
  tkgrid(tklabel(top, text = "r₁  "), r1Entry, sticky = "se")
  tkgrid(tklabel(top, text = "α"), alfaEntry, sticky = "e")
  tkgrid(tklabel(top, text="Parâmetros da espécie 2 :", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "População inicial (N₀₂)  "), n02Entry, sticky = "e")
  tkgrid(tklabel(top, text = "K₂  "), k2Entry, sticky = "e")
  tkgrid(tklabel(top, text = "r₂  "), r2Entry, sticky = "se")
  tkgrid(tklabel(top, text = "β"), betaEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Diagrama de fases"), extBox, sticky = "e")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  tkgrid.configure(tmaxEntry, sticky = "w")
  tkgrid.configure(n01Entry, sticky = "w")
  tkgrid.configure(n02Entry, sticky = "w")
  tkgrid.configure(k1Entry, sticky = "w")
  tkgrid.configure(k2Entry, sticky = "w")
  tkgrid.configure(r1Entry, sticky = "w")
  tkgrid.configure(r2Entry, sticky = "w")
  tkgrid.configure(alfaEntry, sticky = "w")
  tkgrid.configure(betaEntry, sticky = "w")
  tkgrid.configure(extBox, sticky = "w")
  dialogSuffix(rows = 11, columns = 2, focus = tmaxEntry)
}
##########################################
######################################
model4=function(n01,n02,tmax,a,b,c,d)
{
  library(deSolve)  
  # time sequence 
  time <- seq(0, tmax, by = 0.01)
  
  # parameters: a named vector
  parameters <- c(r = a, k = b, e = c, d = d)
  
  # initial condition: a named vector
  state <- c(V = n01, P = n02)
  
  # R function to calculate the value of the derivatives at each time value
  # Use the names of the variables as defined in the vectors above
  lotkaVolterra <- function(t, state, parameters){
    with(as.list(c(state, parameters)), {
      dV = r * V - k * V * P
      dP = e * k * V * P - d * P
      return(list(c(dV, dP)))
    })
  }
  ## Integration with 'ode'
  out <- ode(y = state, times = time, func = lotkaVolterra, parms = parameters)
  
  ## Ploting
  out.df = as.data.frame(out) # required by ggplot: data object must be a data frame
  #dev.new()
  #old=par(mfrow=c(1,2), mar=c(4,4,2,1))
  #if(!df){
    plot(out.df$time,out.df$V,ylim=c(0,max(na.omit(out.df$V))),type="l",lty=4,xlab="time (t)",ylab="Population size", main="Population Growth", col="blue", lwd=1.5 )
    legend("topleft", legend=c("Sp. 1", "Sp. 2"), lty=4, col=c("blue", "green"), bty="n", cex=0.8)
    lines(out.df$time,out.df$P, col="green", lty=4, lwd=1.5)
  #}
  #else{
    #plot(resulta[,2],resulta[,3],type="l",col="red",xlab="N1",ylab="N2",ylim=c(0,max(c(na.omit(resulta[,3]),k1/alfa,k2))),xlim=c(0,max(c(na.omit(resulta[,2]),k2/beta,k1))), man="Diagrama de Fases")
    #points(resulta[length(resulta[,2]),2], resulta[length(resulta[,3]),3])
    #legend("topright",legend=c("Populations trajectory"), lty=c(4,4,1), col=c("red"), bty="n", cex=0.8)
  #}
  #invisible(resulta)
}

predDb<-function () 
{
  dialogName<-"predDb" ### 
  def <- list(n01Var=10, n02Var=10, k1Var= 0.9, k2Var= 0.05, alfaVar= 0.1, betaVar= 0.6, tmaxVar=50) # lista de argumentos padrao
  initial <- getDialog(dialogName, defaults= def)
  initializeDialog(title = gettextRcmdr("Modelo 3"))
  n01Var <- tclVar(initial$n01Var)
  n01Entry <- tkentry(top, width = "4", textvariable = n01Var)
  n02Var <- tclVar(initial$n02Var)
  n02Entry <- tkentry(top, width = "4", textvariable = n02Var)
  k1Var <- tclVar(initial$k1Var)
  k1Entry <- tkentry(top, width = "4", textvariable = k1Var)
  k2Var <- tclVar(initial$k2Var)
  k2Entry <- tkentry(top, width = "4", textvariable = k2Var)
  alfaVar <- tclVar(initial$alfa)
  alfaEntry <- tkentry(top, width = "6", textvariable = alfaVar)
  betaVar <- tclVar(initial$beta)
  betaEntry <- tkentry(top, width = "6", textvariable = betaVar)
  tmaxVar <- tclVar(initial$tmaxVar)
  tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
  onOK <- function() 
  {
    closeDialog()
    n01 <- round(as.numeric(tclvalue(n01Var)))
    if (is.na(n01) || n01 < 0) 
    {
      errorCondition(message = "Number of individuals at the simulation start can't be negative")
      return()
    }
    n02 <- round(as.numeric(tclvalue(n02Var)))
    if (is.na(n02) || n02 < 0) 
    {
      errorCondition(message = "Number of individuals at the simulation start can't be negative")
      return()
    }
    tmax <- round(as.numeric(tclvalue(tmaxVar)))
    if (is.na(tmax) || tmax <= 0) 
    {
      errorCondition("Number of simulations must be a positive integer")
      return()
    }
    k1 <- as.numeric(tclvalue(k1Var))
    
    k2 <- as.numeric(tclvalue(k2Var))
    
    alfa=as.numeric(tclvalue(alfaVar))
    beta=as.numeric(tclvalue(betaVar))
    command <- paste("model4(n01= ",n01, ",n02= ",n02, ", a = ", k1,", c = ", k2,", b = ", alfa,", d = ", beta,", tmax =", tmax, ")", sep = "")
    justDoIt("dev.new()")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, values = list(n01Var=round(as.numeric(tclvalue(n01Var))), n02Var=round(as.numeric(tclvalue(n02Var))), k1Var= as.numeric(tclvalue(k1Var)), k2Var= as.numeric(tclvalue(k2Var)), alfaVar= as.numeric(tclvalue(alfaVar)), betaVar= as.numeric(tclvalue(betaVar)), tmaxVar=as.numeric(tclvalue(tmaxVar)) ), resettable = FALSE)
  }
  OKCancelHelp(helpSubject = "modelo4", reset=dialogName, apply=dialogName)
  tkgrid(tklabel(top, text = "Tempo máximo   "), tmaxEntry, sticky = "e")
  tkgrid(tklabel(top, text="Parâmetros da espécie 1 : ", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "População inicial (N₀₁)  "), n01Entry, sticky = "e")
  tkgrid(tklabel(top, text = "a  "), k1Entry, sticky = "e")
  tkgrid(tklabel(top, text = "b"), alfaEntry, sticky = "e")
  tkgrid(tklabel(top, text="Parâmetros da espécie 2 :", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "População inicial (N₀₂)  "), n02Entry, sticky = "e")
  tkgrid(tklabel(top, text = "c  "), k2Entry, sticky = "e")
  tkgrid(tklabel(top, text = "d"), betaEntry, sticky = "e")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  tkgrid.configure(tmaxEntry, sticky = "w")
  tkgrid.configure(n01Entry, sticky = "w")
  tkgrid.configure(n02Entry, sticky = "w")
  tkgrid.configure(k1Entry, sticky = "w")
  tkgrid.configure(k2Entry, sticky = "w")
  tkgrid.configure(alfaEntry, sticky = "w")
  tkgrid.configure(betaEntry, sticky = "w")
  dialogSuffix(rows = 11, columns = 2, focus = tmaxEntry)
}