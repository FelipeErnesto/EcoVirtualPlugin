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
popExpDb<-function()
{
  dialogName<-"popExpDb" ### inserido
  def <- list(dsname="Do_Not_Save", noVar=10, lambVar= 1.05,tmaxVar=10,inttVar=1) # inserido
  initial <- getDialog(dialogName, defaults= def)
  initializeDialog(title = gettextRcmdr("Exponential Growth"))
  #### Salva dados
  dsname <- tclVar(initial$dsname)
  entryDsname <- tkentry(top, width="20", textvariable=dsname)
  ####
  noVar <- tclVar(initial$noVar)
  noEntry <- tkentry(top, width = "4", textvariable = noVar)
  lambVar <- tclVar(initial$lambVar)
  tmaxVar <- tclVar(initial$tmaxVar)
  tmaxEntry <- tkentry(top, width = "4", textvariable = tmaxVar)
  inttVar<-tclVar(initial$inttVar)
  #########################
  ## 	set.gr=function(...)
  ## 	{
  ## #extVF <- as.logical(as.numeric(tclvalue(extVar)))
  ## 	command=paste("popExp(N0 = ", as.numeric(tclvalue(noVar)), ", lamb = ", as.numeric(tclvalue(lambVar)), ", tmax = ", as.numeric(tclvalue(tmaxVar)),", intt = ", as.numeric(tclvalue(inttVar)),")", sep="")
  ## 	doItAndPrint(command)
  ## #	tkfocus(CommanderWindow())
  ## 	}
  ##########################
  lambEntry<-tkscale(top, from=0, to=5, showvalue=TRUE, variable=lambVar, resolution=0.01, orient="horizontal") #, command=set.gr)
  inttEntry <- tkscale(top, from=0.01, to=1, showvalue=TRUE, variable=inttVar, resolution=0.01, orient="horizontal")#,command=set.gr)
  ################################################
  onOK <- function() 
  {
    closeDialog()
    ############ Data name
    dsnameValue <- trim.blanks(tclvalue(dsname))
    if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
    {
      command <- paste("popExp(N0 = ", as.numeric(tclvalue(noVar)), ", lamb = ", as.numeric(tclvalue(lambVar)), ", tmax = ", as.numeric(tclvalue(tmaxVar)),", intt = ", as.numeric(tclvalue(inttVar)),")", sep="")
    }
    else  
    {
      command <- paste(dsnameValue, " <- popExp(N0 = ", as.numeric(tclvalue(noVar)), ", lamb = ", as.numeric(tclvalue(lambVar)), ", tmax = ", as.numeric(tclvalue(tmaxVar)),", intt = ", as.numeric(tclvalue(inttVar)),")", sep="")
    }
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, values = list(dsname=dsnameValue, noVar=as.numeric(tclvalue(noVar)), lambVar=as.numeric(tclvalue(lambVar)), tmaxVar=as.numeric(tclvalue(tmaxVar)), inttVar=as.numeric(tclvalue(inttVar))), resettable = FALSE) ## inserido
  }
  #popExp(N0,lamb,tmax, intt= 1) 
  OKCancelHelp(helpSubject = "dynPop", reset=dialogName, apply=dialogName) # modificado
  tkgrid(tklabel(top, text="Enter name for last simulation data set: "), entryDsname, sticky="e")
  #tkgrid(tklabel(top, text="Simulation Arena Conditions :  ", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "Maximum time"), tmaxEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Interval time size "), inttEntry, sticky = "e")
  tkgrid(tklabel(top, text="Species parameters :", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "Initial population size  "), noEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Population growth rate (lambda)  "), lambEntry, sticky = "e")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  #tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
  tkgrid.configure(entryDsname, sticky = "w")
  tkgrid.configure(tmaxEntry, sticky = "w")
  tkgrid.configure(inttEntry, sticky = "w")
  tkgrid.configure(noEntry, sticky = "w")
  tkgrid.configure(lambEntry, sticky = "w")
  dialogSuffix(rows = 6, columns = 2, focus = tmaxEntry)
}
############################################################
popLogDb<-function () 
{
  dialogName<-"popLogDb" 
  def <- list(dsname="Do_Not_Save", noVar=10, rVar= 0.05, kVar=100, tmaxVar=100, extVar=0)
  initial <- getDialog(dialogName, defaults= def)
  initializeDialog(title = gettextRcmdr("Logistic Growth"))
  #### Salva dados
  dsname <- tclVar(initial$dsname)
  entryDsname <- tkentry(top, width="20", textvariable=dsname)
  ####
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
  rEntry <-tkscale(top, from=-5, to=5, showvalue=TRUE, variable=rVar, resolution=0.01, orient="horizontal")
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
    ############ Data name
    dsnameValue <- trim.blanks(tclvalue(dsname))
    if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
    {
      command <- paste("popLog(N0= ",N0, ", r = ", r,", K = ", K,", tmax =", tmax, ", ext =", as.logical(as.numeric(tclvalue(extVar))),")", sep = "")
    }
    else  
    {
      command <- paste(dsnameValue,"<-popLog(N0= ",N0, ", r = ", r,", K = ", K,", tmax =", tmax, ", ext =", as.logical(as.numeric(tclvalue(extVar))),")", sep = "")
    }
    ########
    putDialog(dialogName, values = list(dsname= dsnameValue, noVar=as.numeric(tclvalue(noVar)), rVar= as.numeric(tclvalue(rVar)), kVar= as.numeric(tclvalue(kVar)) , tmaxVar=as.numeric(tclvalue(tmaxVar)), extVar=as.logical(as.numeric(tclvalue(extVar)))), resettable = FALSE)
    doItAndPrint(command)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "dynPop", reset=dialogName, apply=dialogName)
  tkgrid(tklabel(top, text="Enter name for data set: "), entryDsname, sticky="e")
  #tkgrid(tklabel(top, text="Simulation Arena Conditions : ", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "Maximum time "), tmaxEntry, sticky = "e")
  tkgrid(tklabel(top, text="Species parameters: ", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "Initial population size "), noEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Carrying capacity (K) "), kEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Intrinsic growth rate (r) "), rEntry, sticky = "se")
  tkgrid(tklabel(top, text = "Population Extinction"), extBox, sticky = "e")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  tkgrid.configure(entryDsname, sticky = "w")
  tkgrid.configure(tmaxEntry, sticky = "w")
  tkgrid.configure(noEntry, sticky = "w")
  tkgrid.configure(kEntry, sticky = "w")
  tkgrid.configure(rEntry, sticky = "w")
  tkgrid.configure(extBox, sticky = "w")
  dialogSuffix(rows = 6, columns = 2, focus = tmaxEntry)
} 
##########################################
##########################################
compDb<-function () 
{
  dialogName<-"compDb" ### 
  def <- list(dsname="Do_Not_Save", n01Var=10, n02Var=10, r1Var= 0.05, r2Var= 0.05, k1Var= 100, k2Var= 100, alfaVar= 1.2, betaVar= 0.5, tmaxVar=100) # lista de argumentos padrao
  initial <- getDialog(dialogName, defaults= def)
  initializeDialog(title = gettextRcmdr("Competition LV Model"))
  #### Salva dados
  dsname <- tclVar(initial$dsname)
  entryDsname <- tkentry(top, width="20", textvariable=dsname)
  ####
  n01Var <- tclVar(initial$n01Var)
  n01Entry <- tkentry(top, width = "4", textvariable = n01Var)
  n02Var <- tclVar(initial$n02Var)
  n02Entry <- tkentry(top, width = "4", textvariable = n02Var)
  r1Var <- tclVar(initial$r1Var)
  r1Entry=tkscale(top, from=-5, to=5, showvalue=TRUE, variable=r1Var, resolution=0.01, orient="horizontal")
  r2Var <- tclVar(initial$r2Var)
  r2Entry=tkscale(top, from=-5, to=5, showvalue=TRUE, variable=r2Var, resolution=0.01, orient="horizontal")
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
    if (is.na(n01) || n01 <= 0) 
    {
      errorCondition(message = "Number of individuals at the simulation start must be a positive integer")
      return()
    }
    n02 <- round(as.numeric(tclvalue(n02Var)))
    if (is.na(n02) || n02 <= 0) 
    {
      errorCondition(message = "Number of individuals at the simulation start must be a positive integer")
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
      errorCondition(message = "Carrying Capacity (K) must be a positive integer")
      return()
    }
    k2 <- as.numeric(tclvalue(k2Var))
    if (is.na(k2) || k2 <= 0)
    {
      errorCondition(message = "Carrying Capacity (K) must be a positive integer")
      return()
    }
    r1=as.numeric(tclvalue(r1Var))
    r2=as.numeric(tclvalue(r2Var))
    alfa=as.numeric(tclvalue(alfaVar))
    beta=as.numeric(tclvalue(betaVar))
    ############ Data name
    dsnameValue <- trim.blanks(tclvalue(dsname))
    if (dsnameValue == "Do_Not_Save" | dsnameValue=="") 
    {
      command <- paste("compLV(n01= ",n01, ",n02= ",n02, ", r1 = ", r1,", r2 = ", r2,", k1 = ", k1,", k2 = ", k2,", alfa = ", alfa,", beta = ", beta,", tmax =", tmax,")", sep = "")
    }
    else  
    {
      command <- paste(dsnameValue, " <- compLV(n01= ",n01, ", n02= ",n02, ", r1 = ", r1,", r2 = ", r2,", k1 = ", k1,", k2 = ", k2,", alfa = ", alfa,", beta = ", beta,", tmax =", tmax,")", sep = "")
    }
    ########
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    putDialog(dialogName, values = list(dsname="Do_Not_Save", n01Var=round(as.numeric(tclvalue(n01Var))), n02Var=round(as.numeric(tclvalue(n02Var))), r1Var= as.numeric(tclvalue(r1Var)), r2Var= as.numeric(tclvalue(r2Var)), k1Var= as.numeric(tclvalue(k1Var)), k2Var= as.numeric(tclvalue(k2Var)), alfaVar= as.numeric(tclvalue(alfaVar)), betaVar= as.numeric(tclvalue(betaVar)), tmaxVar=as.numeric(tclvalue(tmaxVar))), resettable = FALSE)
  }
  OKCancelHelp(helpSubject = "compLV", reset=dialogName, apply=dialogName)
  tkgrid(tklabel(top, text="Enter name for data set:"), entryDsname, sticky="e")
  tkgrid(tklabel(top, text = "Maximum time   "), tmaxEntry, sticky = "e")
  tkgrid(tklabel(top, text="Best competitor species parameters : ", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "Initial population  "), n01Entry, sticky = "e")
  tkgrid(tklabel(top, text = "Carrying capacity (K)  "), k1Entry, sticky = "e")
  tkgrid(tklabel(top, text = "Intrinsic growth rate  "), r1Entry, sticky = "se")
  tkgrid(tklabel(top, text = "Alpha coefficiente"), alfaEntry, sticky = "e")
  tkgrid(tklabel(top, text="Worse Competitor Species :", fg="blue"), sticky="w")
  tkgrid(tklabel(top, text = "Initial population  "), n02Entry, sticky = "e")
  tkgrid(tklabel(top, text = "Carrying capacity (K)  "), k2Entry, sticky = "e")
  tkgrid(tklabel(top, text = "Intrinsic growth rate  "), r2Entry, sticky = "se")
  tkgrid(tklabel(top, text = "Beta coefficiente"), betaEntry, sticky = "e")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  tkgrid.configure(entryDsname, sticky = "w")
  tkgrid.configure(tmaxEntry, sticky = "w")
  tkgrid.configure(n01Entry, sticky = "w")
  tkgrid.configure(n02Entry, sticky = "w")
  tkgrid.configure(k1Entry, sticky = "w")
  tkgrid.configure(k2Entry, sticky = "w")
  tkgrid.configure(r1Entry, sticky = "w")
  tkgrid.configure(r2Entry, sticky = "w")
  tkgrid.configure(alfaEntry, sticky = "w")
  tkgrid.configure(betaEntry, sticky = "w")
  dialogSuffix(rows = 11, columns = 2, focus = tmaxEntry)
}
##########################################
######################################
