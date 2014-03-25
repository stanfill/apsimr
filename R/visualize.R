#' Visualize and APSIM run
#' 
#' @name plot_apsim
#' @param x variable to plot on x-axis
#' @param y variable to plot on y-axis
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
#' plot_apsim(x='Date',data=results[[2]])

plot_apsim<-function(x, y=NULL, data, ...){
  ncol<-ncol(data)
  cNames<-colnames(data)
  idNum<-which(x%in%cNames)
  
  if(is.null(y)){
    y<-cNames[-id.num]
  }else{
    y<-y%in%cNames[-id.num]
  }

  
  for(i in y){
    Sys.sleep(1)
    print(qplot(data[,id.num],data[,i],xlab=x,ylab=i,...)+theme_bw())
  }
}