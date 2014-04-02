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
    
    #For now identify ouput files by assuming they have the same name as the .apsim files
    #Try to be more general about this by going into the .apsim file and grabbing the name of the ouput file
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
  
  if(nOutFiles==1)return(res)
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


#' Edit an APSIM simulation
#' 
#' Edit bits and pieces of an APSIM simulation to automate running
#' 
#' @name edit_apsim
#' @param file file ending in ".apsim" to be edits
#' @param varP parent variables to be edited
#' @param varC child variable to be edited
#' @param value new values for the defined variables
#' @param overwrite T/F depending on if the old file should be over written or a new one should be saved
#' @return nothing, new .apsim file
#' @examples
#' wd <- "C:/Users/Sta36z/Documents/APSIM"
#' 
#' #The file I want to edit is called "Canopy.apsim"
#' file <- "Canopy.apsim"
#' 
#' #I want to change the Thickness of the Soilwater, the SoilCN of the SoilOrganicMatter and
#' #the state at which the simulation is being run.
#' #Because "State" is not nested under anything, the corresponding child variable 
#' #is set to "NA"
#' var<-c("SoilWater/Thickness","SoilOrganicMatter/SoilCN","State")
#' 
#' #Change SoilWater-Thickness to 200,200,300x9
#' #Change SoilCN to 10
#' #Change "State" to "NSW"
#' value<-list(c(rep(200,2),rep(300,9)),10,"NSW")
#' 
#' #Edit the apsim file without overwriting it
#' edit_apsim(file,var,value,overwrite=F)
#' 
#' #Run the edited simulation
#' exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
#' results <- apsimr(exe, getwd(), files = "Canopy-edited.apsim")

edit_apsim<-function(file,var,value,overwrite=T){
  
  if(!(file%in%list.files())){
    stop("Specified file could not be found in the current working directory.")
  }
  
  pXML<-xmlParse(file)
  
  for(i in 1:length(var)){
    
    vari<-pXML[[paste("//",var[i],sep="")]]

    
    for(j in 1:xmlSize(vari)){
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