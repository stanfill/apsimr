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
#' exe <-" \"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe\" "
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' toRun <- c("Centro.apsim","Continuous Wheat.apsim")
#' results <- apsim(exe, wd, files = toRun)

apsim<-function(exe, wd, files = NULL){
  oldWD<-getwd()
  setwd(wd)
  fList<-dir()
  fileNames<-fList[grep(".apsim",fList)]
  
  if(is.null(files)){
    
    #If files is left NULL then run every .apsim file in the provided directory
    files<- fileNames
    
  }else{
    
    #Allow for abbreviations and check the files are in there
    files<-match.arg(files,fileNames,several.ok=TRUE)
    
  }
  
  nFiles<-length(files)
  out_files<-rep(NA,nFiles)
  
  for(i in 1:nFiles){  
    system(paste(exe, files[i], sep = " "), show.output.on.console = FALSE)
    #Grab the name of the ouput file from the simulation file
    out_files[i]<-paste(xmlAttrs(xmlParse(files[i])[["//simulation"]])[[1]],".out",sep="")
  }
  

  results<-vector("list",nFiles)
  skipline<-1
  for(i in 1:nFiles){
    res<-try(read.table(out_files[i],skip=skipline,header=T),TRUE)
    
    while(class(res)=="try-error" & skipline < 50){
      skipline<-skipline+1
      res<-try(read.table(out_files[i],skip=skipline,header=T),TRUE)
    }
    
    res<-res[-1,]
    
    if("Date"%in%colnames(res)) res$Date<-dmy(res$Date)
    
    for(j in 2:ncol(res)){
      res[,j]<-as.numeric(as.character(res[,j])) #Coerce each output to be numeric
    }
    results[[i]]<-res
  }
  
  setwd(oldWD)
  
  if(nFiles==1)return(res)
  else return(results)
  
  
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
#' exe <-" \"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe\" "
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' file <- "Canopy.apsim"
#' apsimEX(path, wd, file)
#' 
#' results <- apsim(exe, wd, files = file)

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
