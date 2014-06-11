\name{apsim}
\alias{apsim}
\title{Run APSIM simulations from R}
\usage{
apsim(exe, wd = getwd(), files = NULL)
}
\arguments{
  \item{exe}{path to the APSIM executable file}

  \item{wd}{working directory containing the .apsim files
  to be run}

  \item{files}{.apsim files to be run.  If left empty all
  .apsim files in \code{wd} will be run}
}
\value{
list of output files corresponding to each .apsim file
}
\description{
This function will run an APSIM from R.  The only required
input is the file path to the APSIM executable.  It is
assumed the current working directory contains the .apsim
file(s) that is to be run.  If that is not the case then
the directory containing the .apsim files to be run needs
to be specified by \code{wd}.  One can specify a list of
.apsim files to be run within the directory \code{wd} using
the \code{files} arguement, otherwise all .apsim files
within \code{wd} are run. Each .apsim file that is run is
an element of the list that is returned.  Each element of
the list has class \code{apsim}.
}
\examples{
\dontrun{
exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
wd <- "~/APSIM"
to_run <- c("Centro.apsim", "Continuous Wheat.apsim")
results <- apsim(exe, wd, files = to_run)
}
}

