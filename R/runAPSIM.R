#' Connect to APSIM
#' 
#' This function will call APSIM from R.
#' 
#' @name apsimr
#' @param exe The path to the APSIM executable file
#' @param wd The working directory containing the .apsim files to be run
#' 
#' @examples
#' exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' results <- apsimr(exe, wd)

apsimr<-function(exe, wd){
  setwd(wd)
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


