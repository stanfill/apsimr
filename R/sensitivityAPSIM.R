#'Edit and run an APSIM simulation
#'
#'This function will both edit and run the edited APSIM simulation.  This function is primarily for use
#'with the sensitivity package but it can be useful on its own as well.
#'
#'
#' @param X N-by-p matrix of inputs with rows corresponding to runs and columns for variables
#' @param exe where to find the APSIM executable
#' @param wd directory where the .apsim file lives and where the results will be saved
#' @param vars names of the variables, must be of the same length as 'X' has columns
#' @param toRun the .apsim file in \code{wd} to run
#' @param toEdit the .apsim file or .xml file to be edited
#' @param g a function of the output returned by apsim - must give univariate result
#' @return a vector of univariate responses
#' @export
#' @examples
#' \dontrun{
#' g<-function(X){
#'  return(mean(X$lai_cowpea))
#' }
#' 
#' wd <- "~/APSIM"
#' var <- c(rep("SoilWater/Thickness",11), "SoilOrganicMatter/SoilCN")
#' value <- matrix(c(rep(200, 2), rep(300, 9), 10,rep(350, 2), rep(350, 9), 5),nrow=2,byrow=T)
#' exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
#' file <- "Canopy.apsim"
#' 
#' res <- apsimSEN(X=value,exe=exe, wd=wd,vars=var,toRun=file, toEdit=file,g=g)
#' res
#' }

apsimSEN<-function(X, exe, wd, vars, toRun, toEdit=toRun, overwrite=FALSE, g){
  #This is a version of the 'apsim' function that can be 
  #used with the sensitivity package
  oldWd<-getwd()
  setwd(wd)
  if(ncol(X)!=length(vars)){
    stop("X must have same number of columns as 'var' is long")
  }
  
  N <- nrow(X)
  y <- rep(0,N)
  
  #Some variables have multiple values that need to be changed, for example
  #soil properties often need to be specified at multiple depths
  unVar<-unique(vars)
  numVar <- length(unVar)
  valueI<-vector("list",numVar)
  
  #When overwrite==FALSE then an new .apsim file is created with the title "___-edited.apsim"
  #Therefore, if toRun==toEdit and overwirte==FALSE then the unchanged version of toEdit is executed
  #This updates the file name so the correct (new) .apsim file is run
  if(!overwrite & toEdit==toRun){
    newName<-paste(gsub(".apsim","",toRun),"-edited",sep="")
    toRun<-paste(newName,".apsim",sep="")
  }
  
  for(i in 1:N){
    
    for(j in 1:numVar){
      valueI[[j]] <- X[i,which(vars==unVar[j])]
    }
    
    if(length(grep(".apsim$",toEdit))>0){
      #edit the .apsim file
      edit_apsim(file=toEdit,wd=wd,var=unVar,value=valueI,overwrite=overwrite)
    }else{
      edit_sim_file(file=toEdit,wd=wd,var=unVar,value=valueI,overwrite=overwrite)
    }

    #Run the edited apsim file and collect the output
    res <- apsim(exe=exe, wd=wd, files = toRun)
    y[i] <- g(res)
  }
  setwd(oldWd)
  return(y)
}