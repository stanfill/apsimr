#' Visualize and APSIM run
#' 
#' @name plot_apsim
#' @param data Object of class .apsim to be plotted
#' @param var Variable to be visualized
#' @param ... additional arguments passed to \code{\link[ggplot2:qplot]{qplot}}
#' @return nothing is returned
#' @S3method plot apsim
#' @method plot apsim
#' @export
#' @examples
#' exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' toRun <- c("Centro.apsim","Continuous Wheat.apsim")
#' results <- apsimr(exe, wd, files = toRun)
#' plot_apsim(results[[2]])

plot_apsim<-function(data, idVar="Date", measureVar=NULL, ...){
  ncol<-ncol(data)
  cNames<-colnames(data)
  idNum<-which(idVar%in%cNames)
  
  if(is.null(measureVar)){
    measureVar<-cNames[-id.num]
  }else{
    measureVar<-measureVar%in%cNames[-id.num]
  }

  
  for(i in measureVar){
    Sys.sleep(1)
    print(qplot(data[,id.num],data[,i],xlab=idVar,ylab=i,...))
  }
}