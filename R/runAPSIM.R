#' Run APSIM simulations from R
#' 
#' 
#' This function will call APSIM from R.  Currently, one needs to supply the 
#' directory containing the APSIM executable and the folder containing the
#' .apsim files that are to be run.  The output from the .apsim files is resturned 
#' in a list: each item in the list corresponds to a .apsim file in the provided directory.
#' 
#' @name apsim
#' @param exe The path to the APSIM executable file
#' @param wd The working directory containing the .apsim files to be run
#' @param files The .apsim files to be run
#' @return list of output files corresponding to each .apsim file
#' @export
#' @examples
#' exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' results <- apsimr(exe, wd)

apsimr<-function(exe, wd, files = NULL){
  setwd(wd)
  
  if(is.null(files)){
    
    sim_file_name<- " *.apsim"
    
  }else{
    
    flist<-list.files()
    possibles <- flist[grep(".apsim",flist)]
    
    if(!all(files %in% possibles)){
      stop("One or more of the requested simulations are not in the specified folder.")
    }
    sim_file_name<-files
    
  }
  
  system(paste(exe, sim_file_name, sep = " "), show.output.on.console = FALSE)
  flist<-list.files()
  
  fileNames<-grep(".out",flist)
  nfiles<-length(fileNames)
  
  outNames<-flist[fileNames]
  
  out_file<-vector("list",nfiles)
  
  for(i in 1:nfiles){
    out_file[[i]]<-read.table(outNames[i],skip=2,header=T)
    
    out_file[[i]]<-out_file[[i]][-1,]
    out_file[[i]][,1]<-factor(out_file[[i]][,1]) #Remove factor level (dd/mm/yyyy)
    
    for(j in 2:ncol(out_file[[i]])){
      out_file[[i]][,j]<-as.numeric(as.character(out_file[[i]][,j])) #Coerce each output to be numeric
    }
  }
  
  return(out_file)
}


