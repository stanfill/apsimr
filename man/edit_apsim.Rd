\name{edit_apsim}
\alias{edit_apsim}
\title{Edit an APSIM simulation}
\usage{
edit_apsim(file, var, value, overwrite = FALSE)
}
\arguments{
  \item{file}{file ending in .apsim to be edited}

  \item{var}{vector of variables to be edited}

  \item{value}{list of new values for the defined
  variables}

  \item{overwrite}{logical; if \code{TRUE} the old file is
  overwritten, a new file is written otherwise}
}
\value{
nothing, new .apsim file
}
\description{
Edit bits and pieces of an APSIM simulation to automate
running
}
\examples{
\dontrun{
#The file I want to edit is called "Canopy.apsim"
file <- "Canopy.apsim"

#I want to change the Thickness of the Soilwater, the SoilCN of the SoilOrganicMatter and
#the state at which the simulation is being run.
var<-c("SoilWater/Thickness","SoilOrganicMatter/SoilCN","State")

#Change SoilWater-Thickness to 200,200,300x9
#Change SoilCN to 10
#Change "State" to "NSW"
value<-list(c(rep(200,2),rep(300,9)),10,"NSW")

#Edit the apsim file without overwriting it
edit_apsim(file,var,value,overwrite=FALSE)

#Run the edited simulation
exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
wd <- "~/APSIM"
results <- apsim(exe, wd, files = "Canopy-edited.apsim")
}
}

