#' Run APSIM simulations from R
#' 
#' 
#' This function will call APSIM from R.  Currently, one needs to supply the 
#' directory containing the APSIM executable and the folder containing the
#' .apsim files that are to be run.  The output from the .apsim files is resturned 
#' in a list: each item in the list corresponds to a .apsim file in the provided directory.
#' 
#' @name apsimr
#' @param exe The path to the APSIM executable file
#' @param wd The working directory containing the .apsim files to be run
#' @param files The .apsim files to be run
#' @return list of output files corresponding to each .apsim file
#' @export
#' @examples
#' exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' toRun <- c("Centro.apsim","ContinuousWheat.apsim")
#' results <- apsimr(exe, wd, files = toRun)

apsimr<-function(exe, wd, files = NULL){
  oldWD<-getwd()
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
  
  for(i in 1:length(sim_file_name)){  
    system(paste(exe, sim_file_name[i], sep = " "), show.output.on.console = FALSE)
  }
  
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
  setwd(oldWD)
  return(out_file)
}

#' Access the examples built into APSIM
#' 
#' To run an APSIM simulation, it will need to be saved outside of the APSIM/Exaple folder.
#' This location needs to be specified by the user.
#' 
#' 
#' @name apsimEX
#' @param exe The path to the APSIM executable file
#' @param wd The working directory containing the .apsim files to be run
#' @param files Which files to extract from the "Examples" folder
#' @param ... additional arguments passed to \code{\link[base:file.copy]{file.copy}}
#' @return nothing is returned
#' @export
#' @examples
#' path <-"C:/Program Files (x86)/Apsim75-r3008/Examples"
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' file <- "Millet.apsim"
#' apsimEX(path, wd,file)
#' 
#' exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
#' results <- apsimr(exe,wd,files=file)

apsimEX<-function(path, wd, files=NULL,...){
  
  oldWD<-getwd()
  setwd(path)
  flist<-list.files()
  possibles <- flist[grep(".apsim",flist)]
  
  if(is.null(files)){
    
    sim_file_name<-possibles
    
  }else{
    
    if(!all(files %in% possibles)){
      stop("One or more of the requested simulations are not in the specified folder.")
    }
    sim_file_name<-files
    
  }
  
  
  for(i in 1:length(files)){
    fromI<-paste(path,sim_file_name[i],sep="/")
    toI<-paste(wd,sim_file_name[i],sep="/")
    file.copy(from=fromI,to=toI,...)
  }
  setwd(oldWD) #Return to origninal working directory
}
