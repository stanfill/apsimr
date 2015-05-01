#' Run APSIM Simulations from R
#' 
#' This function will run one or many APSIM simulation(s) and read the output into R 
#' in the form of a list.  If the simulation does not run for some reason then 
#' an error is returned.
#' 
#' The only required input is the path to the APSIM executable (APSIM.exe) usually found in the "Model"
#' subfolder of the APSIM installation. By default, it is assumed the current working directory contains the .apsim file(s)
#' to be run.  If that is not the case then the directory containing the .apsim file(s) to be run
#' can to be specified by \code{wd}.  One can specify a list of .apsim files to be run within the
#' directory \code{wd} using the \code{files} argument.  If the \code{files} argument is left blank then all 
#' .apsim files within the directory specified by \code{wd} are run. 
#' The results for each .apsim file that is run is an element of the list that is returned.  
#' Each element of the list is of the class \code{"apsim"}, which has its own \code{print} and \code{plot} routines.
#' 
#' @name apsim
#' @param exe  path to the APSIM executable
#' @param wd  working directory containing the .apsim files to be run; defaults to the current working directory
#' @param files  .apsim files to be run; if left empty all .apsim files in \code{wd} will be run
#' @return list of output files; each element corresponds to an .apsim file
#' @export
#' @examples
#' 
#' \dontrun{
#' apsimExe <-"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe"
#' apsimWd <- "~/APSIM"
#' toRun <- c("Centro.apsim", "Continuous Wheat.apsim")
#' results <- apsim(exe = apsimExe, wd = apsimWd, files = toRun)
#' }

apsim<-function(exe, wd = getwd(), files = NULL){
  
  exe<-addCommas(exe) #If there are spaces in the path to APSIM.exe, they need to be added
  oldWD<-getwd()
  setwd(wd)

  fileNames <- dir(,pattern=".apsim$")

  if(length(fileNames)==0){
    setwd(oldWD)
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
    
    res <- suppressWarnings(system(paste(exe,addCommas(files[i]), sep = " "), show.output.on.console = FALSE))
    
    if(res!=0){
      setwd(oldWD)
      stop("An error occured when trying to run APSIM.  Please check your arguments again, especially the path to APSIM.exe.")
    }
    
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
  
  if(nFiles==1){return(res)
  }else{
    names(results)<-gsub(".apsim$","",files)
    return(results)
  }
  
  
}

#' Access example APSIM simulations
#' 
#' There are quite a few standard APSIM simulations provided in the default APSIM installation.
#' \code{apsim_expample} moves those example files into the working directory \code{wd} so you can run them
#' or edit them using \code{\link{apsim}} and \code{\link{edit_apsim}}, respectively.  Generally the
#' example simulations must be moved because the output file is written to the directory containing
#' the .apsim file and the ability to write in the "Program Files" folder is limited in most cases.
#' 
#' 
#' @name example_apsim
#' @param path path to the APSIM installation
#' @param wd working directory containing the .apsim files to be copied; defaults to the current working directory
#' @param files files to extract from the "Examples" folder
#' @param ... additional arguments passed to \code{\link[base:file.copy]{file.copy}}
#' @return logical; if \code{TRUE} the corresponding file was successfully copied, \code{FALSE} otherwise
#' @export
#' @examples
#' \dontrun{
#' apsimPath <-"C:/Program Files (x86)/Apsim75-r3008/"
#' apsimWd <- "~/APSIM"
#' toRun <- "Canopy.apsim"
#' example_apsim(path = apsimPath, wd = apsimWd, files = toRun) #TRUE
#' 
#' toRun <- c("Canopy.apsim", "Continuous Wheat.apsim")
#' example_apsim(path = apsimPath, wd = apsimWd, files = toRun) #TRUE TRUE
#' 
#' apsimExe <-"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe"
#' results <- apsim(exe = apsimExe, wd = apsimWd, files = toRun)
#' }

example_apsim<-function(path, wd = getwd(), files = NULL,...){
  
  oldWD<-getwd()
  setwd(paste(path,"Examples/",sep="/"))
  
  possibles <- dir(,pattern=".apsim$")
  
  
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
  
  res<-rep(NA,length(files))
  for(i in 1:length(files)){
    fromI<-paste(path,"Examples",sim_file_name[i],sep="/")
    toI<-paste(wd,sim_file_name[i],sep="/")
    res[i]<-file.copy(from=fromI,to=toI,overwrite=TRUE)
  }
  setwd(oldWD) #Return to origninal working directory
  return(res)
}
