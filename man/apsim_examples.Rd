\name{apsim_examples}
\alias{apsim_examples}
\title{Access the examples built into APSIM}
\usage{
apsim_examples(path, wd = getwd(), files = NULL, ...)
}
\arguments{
  \item{path}{The path to root of the APSIM installation}

  \item{wd}{The working directory containing the .apsim
  files to be run.  Defaults to the current working
  directory.}

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
path <-"C:/Program Files (x86)/Apsim76-r3376"
wd <- "~/APSIM"
file <- "Canopy.apsim"
apsim_examples(path=path, wd=wd, file)


exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
results <- apsim(exe, wd, files = file)
}
}

