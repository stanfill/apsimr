\name{apsim}
\alias{apsim}
\title{Run APSIM simulations from R}
\usage{
apsim(exe, wd, files = NULL)
}
\arguments{
  \item{exe}{The path to the APSIM executable file}

  \item{wd}{The working directory containing the .apsim
  files to be run}

  \item{files}{The .apsim files to be run.  If left empty
  all .apsim files in wd will be run}
}
\value{
list of output files corresponding to each .apsim file
}
\description{
This function will call APSIM from R.  Currently, one needs
to supply the directory containing the APSIM executable and
the folder containing the .apsim files that are to be run.
The output from the .apsim files is resturned in a list:
each item in the list corresponds to a .apsim file in the
provided directory.
}
\examples{
\dontrun{
exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
wd <- "~/APSIM"
toRun <- c("Centro.apsim","Continuous Wheat.apsim")
results <- apsim(exe, wd, files = toRun)
}
}

