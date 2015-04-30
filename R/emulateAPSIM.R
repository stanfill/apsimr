#' A function to estimate sensitivity indices based on emulation methods
#' 
#' This is a generic function that can be used to estimate the sensitivity
#' indices for complex computer models using GAM-based emulators.
#' 
#' @param model The model that is of interest
#' @param X The matrix of inputs at which to evaluate the model
#' @param y Optional vector of model evaluations that can be used in place of the model statement
#' @param method The method to use to emulate the model
#' @param ... Additional arguments passed to the choosen method
#' @return A data frame of results, the exact form depends on the method
#' @export
#' @examples
#' 
#' \dontrun{
#' meanYield<-function(x){
#' return(mean(x$cowpea.yield))
#' }
#' apsimVars <- c("SoilOrganicMatter/SoilCN", "SoilWater/DiffusConst", "SoilWater/CNCov")
#' n <- 75
#' X1 <- data.frame(SoilCN = runif(n, 5, 25),
#'                  DiffusConst = runif(n, 20, 50), CNCov = runif(n, 0, 1))
#' emulRes <- apsim_emulator(model = apsim_sen, X = X1, method = "singleGAM", exe = apsimExe, wd = apsimWD,
#'                           vars = apsimVars, toRun = file, g = meanYield, overwrite = TRUE)
#' plot(emulRes)
#' 
#' }


apsim_emulator <- function(model, X, y = NULL, method, ...){
  
  method <- try(match.arg(method,c("singleGAM","separateGAM")),silent=TRUE)
  
  if(class(method)=='try-error')
    stop("currently available methods are 'singleGAM' and 'separateGAM'")
  
  if(!is.null(y) & length(y) != nrow(X)){
    stop("The length of y must match the number of rows in X")
  }
  
  if(is.null(colnames(X))){
    colnames(X) <- paste0("V",1:ncol(X))
  }
  
  if(method=='singleGAM'){
    
    res <- single_GAM(model = model, X = X, y = y, ...)
    
  }else if(method == 'separateGAM'){
    
    res <- separate_GAM(model = model, X = X, y = y, ...)
    
  }  
  
  class(res) <- "gamSA"
  
  return(res)
}


single_GAM <- function(model, X, boot = 1000, conf = 0.95, y = NULL, ...){
  
  #This function evaulates the simulator at each row of X,
  #then uses the simulator outputs to fit only one GAM relating every
  #first and second order effect of the columns of X to y.  Then for each
  #input the first order sensitivty index for Xi is estimated by computing
  #the variance of the fitted values when only Xi and the total
  #index is estiamted by the variance of the fitted values
  #when everything except Xi is used.
  
  if(is.null(y)){
    y <- model(X,...)
  }
  p <- ncol(X)
  
  if(nrow(X)<(8*p^2-4*p+1)){
    stop(paste("More data points are needed to fit the single GAM.  Please try again with at least ",8*p^2-4*p+1," rows in X."))
  }
  
  vY <- var(y)
  if(is.null(colnames(X))){
    colnames(X) <- 1:p
    for(i in 1:p){
      colnames(X)[i] <- paste0("X",i)
    }
  }
  
  cnames <- colnames(X)
  ints <- combn(cnames,2)
  XallNames <- c(cnames,rep(NA,ncol(ints)))
  
  for(j in 1:ncol(ints)){
    XallNames[(j+p)] <- paste(ints[1,j],ints[2,j],sep=',')
  }
  
  formi<-paste0("y~ti(",XallNames[1])
  for(j in 2:length(XallNames)){
    formi <- paste0(formi,")+ti(",XallNames[j])
  }
  formi <- formula(paste0(formi,")"))
  
  #Fit the GAM and predict from it, i.e. emulate
  gamFit <- mgcv::gam(formi,data=data.frame(cbind(y,X)))
  #vY <- var(predict(gamFit))
  xStar <- predict(gamFit,type='lpmatrix')
  bhat <- gamFit$coef
  V <- gamFit$Vp
  xstarNames <- colnames(xStar)
  
  Sidf <- STdf <- data.frame(Parameter=cnames,Est=0,SE=0,Bias=0,Lower=0,Upper=0)
  #STdf$EstAlt <- 0
  
  for(i in 1:p){
    #Estimate STi
    xiCols <- grep(cnames[i],xstarNames)
    xStari <- xStar
    xStari[,c(xiCols)] <- 0 #Every column that has to do with Xi is made zero
    ghati <- xStari%*%bhat
    STdf$Est[i] <- 1-var(ghati)/vY
    
    #Try the other way, that is, include all terms involving Xi and use
    #variance explained by that divided by total variance
    #XstariAlt <- xStar
    #XstariAlt[,-c(xiCols)] <- 0
    #ghatiAlt <- XstariAlt%*%bhat
    #STdf$EstAlt[i] <- var(ghatiAlt)/vY
    
    #Bootstrap the bhats to get SE and CIs for total indices
    sample.coef <- MASS::mvrnorm(boot,bhat,V)
    ghats <- sample.coef%*%t(xStari)
    BootVis <- apply(ghats,1,var)
    BootSTis <- 1-BootVis/vY
    STdf$SE[i] <- sd(BootSTis)
    STCis <- quantile(BootSTis,c((1-conf)/2,1-(1-conf)/2))
    STdf$Lower[i] <- STCis[1]
    STdf$Upper[i] <- STCis[2]
    STdf$Bias[i] <- mean(BootSTis) - STdf$Est[i]
    
    #Estimate Si
    xiCols <- grep(paste0("(",cnames[i],")"),xstarNames,fixed=TRUE)
    xStari <- xStar
    xStari[,-c(1,xiCols)] <- 0
    ghati <- xStari%*%bhat
    Sidf$Est[i] <- var(ghati)/vY
    
    #Use the bootstrapped bhats to get SE and CIs for first order indices
    ghats <- sample.coef%*%t(xStari)
    BootVs <- apply(ghats,1,var)
    BootSis <- BootVs/vY
    Sidf$SE[i] <- sd(BootSis)
    SCis <- quantile(BootSis,c((1-conf)/2,1-(1-conf)/2))
    Sidf$Lower[i] <- SCis[1]
    Sidf$Upper[i] <- SCis[2]
    Sidf$Bias[i] <- mean(BootSis) - Sidf$Est[i]
  }
  
  return(list(FirstOrder=Sidf,Total=STdf,ehat=gamFit$residuals,yhat=gamFit$fitted))
}


separate_GAM<-function(model , X, boot = 1000, conf = 0.95, y = NULL,...){
  #model - the function to produce outputs y, can't be left empty but it's not necessary if y is not null
  #y - the vector of length n of model outputs (takes precedence over model argument)
  #X - the n-by-p matrix of input values
  #B - the number of bootstrap replicates to use to estimate SE and confidence interval
  #conf - in interval (0,1), level of confidence of interval to return
  #Use nonparameteric regression to estimate first order sensitivity indices along with
  #parameteric bootstrap SE estimate
  
  if(is.null(y)){
    y <- model(X,...)
  }
  
  nparam <- ncol(X)
  n <- nrow(X)
  Vy <- var(y)
  SiEst <- BootSiSE <- Bias <- rep(0,nparam)
  Cis <- matrix(0,nparam,2)
  resids <- fitted <- matrix(0,nrow(X),ncol(X))
  
  for(i in 1:nparam){
    
    GAMfit <- mgcv::gam(y~s(X[,i]))
    resids[,i] <- GAMfit$residuals
    fitted[,i] <- GAMfit$fitted.values
    
    SiEst[i] <- var(GAMfit$fitted)/Vy
    
    beta.hat <- GAMfit$coef
    V <- GAMfit$Vp
    Xstar <- predict(GAMfit,type='lpmatrix')
    sample.coef <- MASS::mvrnorm(boot,beta.hat,V)
    ghats <- sample.coef%*%t(Xstar)
    
    BootVis <- apply(ghats,1,var)
    BootSis <- BootVis/Vy
    BootSiSE[i] <- sd(BootSis)
    Cis[i,] <- quantile(BootSis,c((1-conf)/2,1-(1-conf)/2))
    Bias[i] <- mean(BootSis) - SiEst[i]
  }
  
  return(list(FirstOrder=data.frame(Parameter=colnames(X),Est=SiEst,SE=BootSiSE,Bias=Bias,Lower=Cis[,1],Upper=Cis[,2]),
              ehat=resids,yhat=fitted)) 
}


#' Plot Sensitivity Analysis Results
#' 
#' The default plot method for the results of a sensitivity analysis of APSIM using
#' the single or separate GAM-based emulator.  Produces a bar chart where the heights of the
#' bar represents the magnitude of the estimated sensitivity index estimate.  Error bars represent
#' bootstrap calibrated confidence regions.
#' 
#' @name plot.gamSA
#' @param x,... Results of a successful call to \code{apsim_emulator}
#' @return The results of the sensitivity analysis without the estimated output and residuals
#' @export

plot.gamSA <- function(x,...){
  saRes <- x
  p <- length(saRes$FirstOrder$Est)
  togDF <- data.frame(Parameter=rep(saRes$FirstOrder$Parameter,2))
  togDF$Estimate <- c(saRes$FirstOrder$Est,saRes$Total$Est)
  togDF$Index <- rep(c("First-order","Total"),each=p)
  togDF$Lower <- c(saRes$FirstOrder$Lower,saRes$Total$Lower)
  togDF$Upper <- c(saRes$FirstOrder$Upper,saRes$Total$Upper)
  dodge <- position_dodge(width=.9)
  
  if(is.null(saRes$Total)){
    
    togDF <- togDF[togDF$Index=="First-order",]
    pp <- qplot(togDF$Parameter,togDF$Estimate,geom='bar',stat='identity')+theme_bw()+
      geom_errorbar(aes(ymin=togDF$Lower, ymax=togDF$Upper),position=dodge,width=.25)+ylab("")+xlab("")
    
  }else{
    
    pp <- qplot(togDF$Parameter,togDF$Estimate,geom='bar',stat='identity',fill=togDF$Index,position='dodge')+
      theme_bw()+geom_errorbar(aes(ymin=togDF$Lower, ymax=togDF$Upper),position=dodge,width=.25)+ylab("")+xlab("")+
      guides(fill=guide_legend(title="Index"))
    
  }
  return(pp)
  
}

#' Print Sensitivity Analysis Results
#' 
#' The default print method for the results of a sensitivity analysis of APSIM using
#' the single or separate GAM-based emulator.
#' 
#' @name print.gamSA
#' @param x,... Results of a successful call to \code{apsim_emulator}
#' @return The results of the sensitivity analysis without the estimated output and residuals
#' @export

print.gamSA <- function(x,...){
  saRes <- x
  saRes$ehat <- NULL
  saRes$yhat <- NULL
  
  print.default(saRes,...)
  
}