#' Run APSIM simulations from R
#' 
#' 
#' This function will call APSIM from R.  The only required input is the 
#' file path to the APSIM executable.  It is assumed the current working directory contains the .apsim file(s)
#' that is to be run.  If that is not the case then the directory containing the .apsim files to be run
#' needs to be specified by \code{wd}.  One can specify a list of .apsim files to be run within the
#' directory \code{wd} using the \code{files} arguement, otherwise all .apsim files within \code{wd} are run. 
#' Each .apsim file that is run is an element of the list that is returned.  Each element of the list has class
#' \code{apsim}.
#' 
#' @name apsim
#' @param exe  path to the APSIM executable file
#' @param wd  working directory containing the .apsim files to be run
#' @param files  .apsim files to be run.  If left empty all .apsim files in \code{wd} will be run
#' @return list of output files corresponding to each .apsim file
#' @export
#' @examples
#' 
#' \dontrun{
#' exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
#' wd <- "~/APSIM"
#' to_run <- c("Centro.apsim", "Continuous Wheat.apsim")
#' results <- apsim(exe, wd, files = to_run)
#' }

apsim<-function(exe, wd = getwd(), files = NULL){
  
  exe<-addCommas(exe) #If there are spaces in the path to APSIM.exe, they need to be added
  oldWD<-getwd()
  setwd(wd)
  fList<-dir()
  fileNames<-fList[grep(".apsim",fList)]
  
  if(length(fileNames)==0){
    stop("There are no .apsim files in the folder wd to run.")
  }
  
  if(is.null(files)){
    
    #If files is left NULL then run every .apsim file in the provided directory
    files<- fileNames
    
  }else{
    
    nFiles<-length(files)
    #Allow for abbreviations and check the files are in there
    files<-match.arg(files,fileNames,several.ok=TRUE)
    if(nFiles != length(files))
      warning("Not all of the requested files could be found in the specified directory")
  }
  
  nFiles<-length(files)
  out_files<-rep(NA,nFiles)
  
  for(i in 1:nFiles){  
    
    system(paste(exe,addCommas(files[i]), sep = " "), show.output.on.console = FALSE)
    
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
    class(res)<-c("apsim","data.frame")
    results[[i]]<-res
  }
  
  setwd(oldWD)
  
  if(nFiles==1)return(res)
  else return(results)
  
  
}

#' Access the examples built into APSIM
#' 
#' There are quite a few standard APSIM simulations provided in the default APSIM installation.
#' \code{apsim_expample} moves those example files into your working directory so you can run them
#' or edit them using \code{\link{apsim}} and \code{\link{edit_apsim}}, respectively.
#' 
#' 
#' @name example_apsim
#' @param path The path to root of the APSIM installation
#' @param wd The working directory containing the .apsim files to be run.  Defaults to the current working directory.
#' @param files Which files to extract from the "Examples" folder
#' @param ... additional arguments passed to \code{\link[base:file.copy]{file.copy}}
#' @return nothing is returned
#' @export
#' @examples
#' 
#' \dontrun{
#' path <-"C:/Program Files (x86)/Apsim76-r3376"
#' wd <- "~/APSIM"
#' file <- "Canopy.apsim"
#' example_apsim(path=path, wd=wd, file)
#' 
#' 
#' exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
#' results <- apsim(exe, wd, files = file)
#' }

example_apsim<-function(path, wd = getwd(), files = NULL,...){
  
  oldWD<-getwd()
  setwd(paste(path,"Examples/",sep="/"))
  
  flist<-list.files()
  possibles <- flist[grep(".apsim",flist)]
  
  if(length(possibles)==0){
    stop("There are no .apsim files in the folder wd to copy.")
  }
  
  if(is.null(files)){
    
    sim_file_name<-possibles
    
  }else{
    
    if(!all(files %in% possibles)){
      stop("One or more of the requested simulations are not in the specified folder.")
    }
    sim_file_name<-files
    
  }
  
  
  for(i in 1:length(files)){
    fromI<-paste(path,"Examples",sim_file_name[i],sep="/")
    toI<-paste(wd,sim_file_name[i],sep="/")
    if(!file.copy(from=fromI,to=toI,overwrite=TRUE)){
      stop("Copy failed.")
    }
  }
  setwd(oldWD) #Return to origninal working directory
}
