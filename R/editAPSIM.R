#' Edit an APSIM Simulation
#' 
#' The variables specified by \code{var} within the .apsim file specified by \code{file} 
#' in the working directory \code{wd} are edited. The old values are replaced with \code{value}, which
#' is a list that has the same number of elements as the length of the vector \code{var}.  The current
#' .apsim file will be overwritten if \code{overwrite} is set to \code{TRUE}; otherwise the file
#' \emph{file-edited.apsim} will be created.  The name of the written file is returned.
#' 
#' @name edit_apsim
#' @param file file ending in .apsim to be edited
#' @param wd directory containing the .apsim file to be edited; defaults to the current wd
#' @param var vector of variables to be edited
#' @param value list of new values for the specified variables
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @return character string containing the resulting XML content
#' @export
#' @examples
#' \dontrun{
#' #The file I want to edit is called "Canopy.apsim" which is in the directory "~/APSIM"
#' file <- "Canopy.apsim"
#' wd <- "~/APSIM"
#' 
#' #I want to change the Thickness of the Soilwater, the SoilCN of the SoilOrganicMatter and
#' #the state at which the simulation is being run.
#' var <- c("SoilWater/Thickness", "SoilOrganicMatter/SoilCN", "State")
#' 
#' #Change SoilWater-Thickness to 200,200,300x9
#' #Change SoilCN to 10
#' #Change "State" to "NSW"
#' value <- list(c(rep(200, 2), rep(300, 9)), 9, "NSW")
#' 
#' #Edit the apsim file without overwriting it
#' edit_apsim(file, wd, var, value, overwrite = FALSE)
#' 
#' #Run the edited simulation
#' exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
#' 
#' results <- apsim(exe, wd, files = "Canopy-edited.apsim")
#' }

edit_apsim <- function(file, wd = getwd(), var, value, overwrite = FALSE){
  
  oldWD<-getwd()
  setwd(wd)
  
  fileNames <- dir(,pattern=".apsim$")
  
  if(length(fileNames)==0){
    stop("There are no .apsim files in the specified directory 'wd' to edit.")
  }
  
  file<-match.arg(file,fileNames,several.ok=TRUE)
  
  pXML<-xmlParse(file)
  
  for(i in 1:length(var)){
    
    vari<-pXML[[paste("//",var[i],sep="")]]
    
    #If supplied length is shorter then length to replace, then
    #leave the remaining values unchanged
    lToReplace<-xmlSize(vari)
    lReplace<-length(value[[i]])
    lenDiff<-lToReplace-lReplace
    
    if(lenDiff>0){
      #value[[i]]<-c(value[[i]],rep(value[[i]][lReplace],lenDiff))
      warning(paste("Only the first",lReplace,"elements were changed",sep=" "))
    }
    
    for(j in 1:lReplace){
      xmlValue(vari[[j]])<-as.character(value[[i]][j])
    }
    
  }
  #Be sure the edited file is written to the specified wd and not the current wd
  addWd <- paste(wd,file,sep="/")
  
  if(overwrite){
    setwd(oldWD)
    return(saveXML(pXML,file=addWd))
  }else{
    
    #Remove .apsim tag if present and add edited tag
    newName<-paste(gsub(".apsim","",addWd),"-edited",sep="")
    
    #Rename the simulation
    wholeSim<-pXML[["//simulation"]]    
    xmlAttrs(wholeSim)<-c(name=newName)
    
    #Rename the output filename to match the new file name
    outName<-pXML[["//outputfile/filename"]]
    xmlValue(outName)<-paste(newName,".out",sep="")
    
    #Also update title for output file
    outTitle<-pXML[["//outputfile/title"]]
    xmlValue(outTitle)<-newName
    
    setwd(oldWD)
    return(saveXML(pXML,file=paste(newName,".apsim",sep="")))
  }
}

#' Edit an APSIM Module File
#' 
#' APSIM uses .xml files to dictate how certain processes are carried out.  Similar to
#' \code{\link{edit_apsim}} this function edits a file that will be used in an APSIM simulation.  Unlike
#' \code{\link{edit_apsim}} this function edits the .xml simulation files.
#' The variables specified by \code{var} within the .xml file specified by \code{file} 
#' in the working directory \code{wd} are edited. The old values are replaced with \code{value}, which
#' is a list that has the same number of elements as the vector \code{var} is long.  The current
#' .xml file will be overwritten if \code{overwrite} is set to \code{TRUE}; otherwise the file
#' \emph{file-edited.xml} will be created.  The name of the written file is returned.
#' 
#' @name edit_sim_file
#' @param file .xml module file to be edited
#' @param wd directory containing the .xml file to be edited; defaults to the current wd
#' @param var vector of variables to be edited
#' @param value list of new values for the specified variables
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @return character string containing the resulting XML content
#' @export
#' @examples
#' \dontrun{
#' #The file I want to edit is called "Soil.xml" which is the the directory "~/APSIM"
#' file <- "Soil.xml"
#' wd <- "~/APSIM"
#' 
#' #I want to change the potential nitrification and N2O from nitrification
#' var <- c("nitrification_pot", "dnit_nitrf_loss","wfnit_values")
#' 
#' #Change both to absolute values of random N(0,1) 
#' value <- list(abs(rnorm(1)), abs(rnorm(1)), c(0,2,2,1))
#' 
#' #Edit Soil.xml without overwriting it
#' edit_sim_file(file, wd, var, value, overwrite = FALSE)
#' }

edit_sim_file <- function(file, wd = getwd(), var, value, overwrite = FALSE){
  
  oldWD<-getwd()
  setwd(wd)
  
  if(!(file%in%list.files())){
    stop("Specified file could not be found in the current working directory.")
  }
  
  pXML<-xmlParse(file)
  
  for(i in 1:length(var)){
    
    vari<-pXML[paste("//",var[i],sep="")]
    
    #If supplied length is shorter then length to replace, then
    #replicate the last value enough times to fill the void, give message
    lengthVari <- xmlSize(vari)
    lReplace<-length(value[[i]])
    
    if(lReplace>1){
      newVar <- as.character(value[[i]][1])
      for(k in 2:lReplace){
        newVar <- paste(newVar,value[[i]][k],sep=" ")
      }
    }else{
      newVar <- as.character(value[[i]])
    }
    
    for(k in 1:lengthVari){
      xmlValue(vari[[k]]) <- newVar
    }
    
  }
  addWd <- paste(wd,file,sep="/")
  if(overwrite){
    setwd(oldWD)
    return(saveXML(pXML,file=addWd))
  }else{
    
    #Remove .apsim tag if present and add edited tag
    newName<-paste(gsub(".xml","",addWd),"-edited",sep="")
    setwd(oldWD)
    return(saveXML(pXML,file=paste(newName,".xml",sep="")))
  }  
}