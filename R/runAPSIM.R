#' Connect to APSIM
#' 
#' This function will call APSIM from R.
#' 
#' @param wd The working directory containing the .apsim files to be run
#' 
#' @examples
#' wd<-"C:/Users/Sta36z/Documents/APSIM"
#' apsimr(wd)

apsimr<-function(wd){
  setwd(wd)
  apsimEXE<-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
  sim_file_name<- " *.apsim"
  system(paste(apsimEXE, sim_file_name, sep = " "), show.output.on.console = FALSE)
  flist<-list.files()
  
  fileNames<-grep(".out",flist)
  nfiles<-length(fileNames)
  
  outNames<-flist[fileNames]
  
  out_file<-vector("list",nfiles)
  
  for(i in 1:nfiles){
    out_file[[i]]<-read.table(outNames[i], header = TRUE, skip=2)
  }
  return(out_file)
}


