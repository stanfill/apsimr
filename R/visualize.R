#' Visualize and APSIM run
#' 
#' @name plot.apsim
#' @param x Data frame of class 'apsim' including the results of an APSIM simulation
#' @param y variable to plot on y-axis.  If left empty all variables will be plotted on seperate plots.
#' @param ask logical; if \code{TRUE}, the user is asked before ach plot, see \code{\link{par}}(ask=.) 
#' @param ... additional arguments passed to \code{\link[ggplot2:qplot]{qplot}}
#' @return nothing is returned
#' @S3method plot apsim
#' @method plot apsim
#' @export
#' @examples
#' \dontrun{
#' exe <-" \"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe\" "
#' wd <- "../APSIM"
#' toRun <- c("Centro.apsim","Continuous Wheat.apsim")
#' results <- apsim(exe, wd, files = toRun)
#' 
#' #Look at all of the results as a function of time
#' plot(results[[2]])
#' 
#' #Plot just yield as a function of time
#' plot(results[[2]],y='yield')+geom_line(colour='red')+theme_bw()
#' }

plot.apsim<-function(x,y=NULL,ask=TRUE,...){
  ncol<-ncol(x)
  cNames<-colnames(x)
  idNum<-which('Date'%in%cNames)
  
  if(is.null(y)){
    y<-cNames[-idNum]
  }else{
    if(!(y%in%cNames)){
      stop(paste(y,"is not an available response."))
    }
  }
  
  if(length(y)==1){

    return(qplot(x[,'Date'],x[,y],xlab='Date',ylab=y,...))
    
  }else{
    
    if(ask){
      oask <- devAskNewPage(TRUE)
    }
    
    for(i in y){
      print(qplot(x[,'Date'],x[,i],xlab='Date',ylab=i,...)+theme_bw())
    }
    oask <- devAskNewPage(FALSE)
  }
}