\name{apsimEX}
\alias{apsimEX}
\title{Access the examples built into APSIM}
\usage{
apsimEX(path, wd, files = NULL, ...)
}
\arguments{
  \item{path}{The path to the APSIM executable file}

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
outside of the APSIM/Exaple folder. This location needs to
be specified by the user.
}
\examples{
\dontrun{
exe <-" \\"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe\\" "
wd <- "C:/Users/Sta36z/Documents/APSIM"
file <- "Canopy.apsim"
apsimEX(path, wd, file)

results <- apsim(exe, wd, files = file)
}
}

