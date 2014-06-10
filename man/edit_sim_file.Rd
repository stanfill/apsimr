\name{edit_sim_file}
\alias{edit_sim_file}
\title{Edit an APSIM simulation simulation file}
\usage{
edit_sim_file(file, var, value, overwrite = F)
}
\arguments{
  \item{file}{file ending in ".xml" to be edits}

  \item{var}{vector of variables to be edited}

  \item{value}{list of new values for the defined
  variables}

  \item{overwrite}{T/F depending on if the old file should
  be over written or a new one should be saved}
}
\value{
nothing, new .xml file
}
\description{
Edit bits and pieces of an APSIM simulation to automate
running
}
\examples{
\dontrun{
wd <- "C:/Users/Sta36z/Documents/APSIM"
setwd(wd)
#The file I want to edit is called "Canopy.apsim"
file <- "Soil2.xml"

#I want to change the potential nitrification and N2O from nitrification
var<-c("nitrification_pot","dnit_nitrf_loss")

#Change both to absolute values of random N(0,1)
value<-list(abs(rnorm(1)),abs(rnorm(1)))

#Edit the apsim file without overwriting it
edit_sim_file(file,var,value,overwrite=F)
}
}

