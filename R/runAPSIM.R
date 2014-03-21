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
#' toRun <- c("Centro.apsim","Continuous Wheat.apsim")
#' results <- apsimr(exe, wd, files = toRun)

apsimr<-function(exe, wd, files = NULL){
  oldWD<-getwd()
  setwd(wd)
  fList<-dir()
  fileNames<-fList[grep(".apsim",fList)]
  
  if(is.null(files)){
    
    #If files is left NULL then run every .apsim file in the provided directory
    files<- " *.apsim"
    out_files <- sub(".apsim",".out",fileNames)
    
  }else{
    #Check that the specified .apsim files exist in the directory
    
    if(!all(files %in% fileNames)){
      stop("One or more of the requested simulations are not in the specified folder.")
    }
    
    out_files <- sub(".apsim",".out",files)
  }
  
  
  for(i in 1:length(files)){  
    system(paste(exe, files[i], sep = " "), show.output.on.console = FALSE)
  }
  

  nOutFiles<-length(out_files)
  results<-vector("list",nOutFiles)
  
  for(i in 1:nOutFiles){
    res<-read.table(out_files[i],skip=2,header=T)
    
    res<-res[-1,]
    
    if("Date"%in%colnames(res)) res$Date<-dmy(res$Date)
    
    for(j in 2:ncol(res)){
      res[,j]<-as.numeric(as.character(res[,j])) #Coerce each output to be numeric
    }
    results[[i]]<-res
  }
  
  setwd(oldWD)
  if(nOutFiles==1) return(res)
  
  else  return(results)
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
#' file <- "Canopy.apsim"
#' apsimEX(path, wd, file)
#' 
#' exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
#' results <- apsimr(exe, wd, files = file)

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
