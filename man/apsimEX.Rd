\name{apsimEX}
\alias{apsimEX}
\title{Access the examples built into APSIM}
\usage{
  apsimEX(path, wd, files = NULL)
}
\arguments{
  \item{exe}{The path to the APSIM executable file}

  \item{wd}{The working directory containing the .apsim
  files to be run}

  \item{files}{Which files to extract from the "Examples"
  folder}

  \item{...}{additional arguments passed to
  \code{\link[base:file.copy]{file.copy}}}
}
\value{
  nothing is returned
}
\description{
  To run an APSIM simulation, it will need to be saved
  outside of the APSIM/Exaple folder. This location needs
  to be specified by the user.
}
\examples{
path <-"C:/Program Files (x86)/Apsim75-r3008/Examples"
wd <- "C:/Users/Sta36z/Documents/APSIM"
file <- "Blank Simulation.apsim"
results <- apsimEX(path, wd)
}

