#' Edit an APSIM simulation
#' 
#' Edit bits and pieces of an APSIM simulation to automate running
#' 
#' @name edit_apsim
#' @param file file ending in .apsim to be edited
#' @param var vector of variables to be edited
#' @param value list of new values for the defined variables
#' @param overwrite logical; if \code{TRUE} the old file is overwritten, a new file is written otherwise
#' @return nothing, new .apsim file
#' @examples
#' \dontrun{
#' #The file I want to edit is called "Canopy.apsim"
#' file <- "Canopy.apsim"
#' 
#' #I want to change the Thickness of the Soilwater, the SoilCN of the SoilOrganicMatter and
#' #the state at which the simulation is being run.
#' var<-c("SoilWater/Thickness","SoilOrganicMatter/SoilCN","State")
#' 
#' #Change SoilWater-Thickness to 200,200,300x9
#' #Change SoilCN to 10
#' #Change "State" to "NSW"
#' value<-list(c(rep(200,2),rep(300,9)),10,"NSW")
#' 
#' #Edit the apsim file without overwriting it
#' edit_apsim(file,var,value,overwrite=FALSE)
#' 
#' #Run the edited simulation
#' exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
#' wd <- "~/APSIM"
#' results <- apsim(exe, wd, files = "Canopy-edited.apsim")
#' }

edit_apsim <- function(file, var, value, overwrite = FALSE){
  
  if(!(file%in%list.files())){
    stop("Specified file could not be found in the current working directory.")
  }
  
  pXML<-xmlParse(file)
  
  for(i in 1:length(var)){
    
    vari<-pXML[[paste("//",var[i],sep="")]]
    
    #If supplied length is shorter then length to replace, then
    #replicate the last value enough times to fill the void, give message
    lToReplace<-xmlSize(vari)
    lReplace<-length(value[[i]])
    lenDiff<-lToReplace-lReplace
    
    if(lenDiff>0){
      value[[i]]<-c(value[[i]],rep(value[[i]][lReplace],lenDiff))
      warning(paste("Supplied values for",var[i],"was too short",sep=" "))
    }
    
    for(j in 1:lToReplace){
      xmlValue(vari[[j]])<-as.character(value[[i]][j])
    }
    
  }
  
  if(overwrite){
    saveXML(pXML,file=file)
  }else{
    
    #Remove .apsim tag if present and add edited tag
    newName<-paste(gsub(".apsim","",file),"-edited",sep="")
    
    #Rename the simulation
    wholeSim<-pXML[["//simulation"]]    
    xmlAttrs(wholeSim)<-c(name=newName)
    
    #Rename the output filename to match the new file name
    outName<-pXML[["//outputfile/filename"]]
    xmlValue(outName)<-paste(newName,".out",sep="")
    
    #Also update title for output file
    outTitle<-pXML[["//outputfile/title"]]
    xmlValue(outTitle)<-newName
    
    saveXML(pXML,file=paste(newName,".apsim",sep=""))
  }
}

#' Edit an APSIM simulation simulation file
#' 
#' Edit bits and pieces of an APSIM simulation to automate running
#' 
#' @name edit_sim_file
#' @param file file ending in ".xml" to be edits
#' @param var vector of variables to be edited
#' @param value list of new values for the defined variables
#' @param overwrite T/F depending on if the old file should be over written or a new one should be saved
#' @return nothing, new .xml file
#' @examples
#' \dontrun{
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' setwd(wd)
#' #The file I want to edit is called "Soil.xml"
#' file <- "Soil.xml"
#' 
#' #I want to change the potential nitrification and N2O from nitrification
#' var <- c("nitrification_pot", "dnit_nitrf_loss")
#' 
#' #Change both to absolute values of random N(0,1) 
#' value <- list(abs(rnorm(1)), abs(rnorm(1)))
#' 
#' #Edit the apsim file without overwriting it
#' edit_sim_file(file, var, value, overwrite = FALSE)
#' }

edit_sim_file <- function(file, var, value, overwrite = FALSE){
  
  if(!(file%in%list.files())){
    stop("Specified file could not be found in the current working directory.")
  }
  
  pXML<-xmlParse(file)
  
  for(i in 1:length(var)){
    
    vari<-pXML[paste("//",var[i],sep="")]
    
    #If supplied length is shorter then length to replace, then
    #replicate the last value enough times to fill the void, give message
    lToReplace<-xmlSize(vari)
    lReplace<-length(value[[i]])
    lenDiff<-lToReplace-lReplace
    
    if(lenDiff>0){
      value[[i]]<-c(value[[i]],rep(value[[i]][lReplace],lenDiff))
      #warning(paste("Supplied values for",var[i],"was too short",sep=" "))
    }
    
    for(j in 1:lToReplace){
      xmlValue(vari[[j]])<-as.character(value[[i]][j])
    }
    
  }
  
  if(overwrite){
    saveXML(pXML,file=file)
  }else{
    
    #Remove .apsim tag if present and add edited tag
    newName<-paste(gsub(".xml","",file),"-edited",sep="")
    
    saveXML(pXML,file=paste(newName,".xml",sep=""))
  }
}