#' @param X N-by-p matrix of inputs with rows corresponding to runs and columns for variables
#' @param exe where to find the APSIM executable
#' @param wd directory where the .apsim file lives and where the results will be saved
#' @param vars names of the variables, must be of the same length as 'X' has columns
#' @param file the .apsim file to be edited and run
#' @param g a function of the output returned by apsim - must give univariate result
#' @examples
#' g<-function(X){
#'  return(mean(X$lai_cowpea))
#' }
#' 
#' wd <- "~/APSIM"
#' var <- c(rep("SoilWater/Thickness",11), "SoilOrganicMatter/SoilCN")
#' value <- matrix(c(rep(200, 2), rep(300, 9), 10,rep(250, 2), rep(350, 9), 12),nrow=2,byrow=T)
#' exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
#' file <- "Canopy.apsim"
#' 
#' res <- apsimSEN(X=value,exe=exe, wd=wd,vars=var,file=file,g=g)

apsimSEN<-function(X, exe, wd, vars, file, overwrite=FALSE, g){
  #This is a version of the 'apsim' function that can be 
  #used with the sensitivity package
  
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
  
  for(i in 1:N){
    
    for(j in 1:numVar){
      valueI[[j]] <- X[i,which(vars==unVar[j])]
    }
    
    #edit the .apsim file
    edit_apsim(file=file,wd=wd,var=unVar,value=valueI,overwrite=overwrite)
    
    #Run the edited apsim file and collect the output
    res <- apsim(exe=exe, wd=wd, files = "Canopy-edited.apsim") #This needs to be changed, otherwise should be OK
    y[i] <- g(res)
  }
  return(y)
}