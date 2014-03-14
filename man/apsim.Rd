\name{apsim}
\alias{apsim}
\title{Run APSIM simulations from R}
\usage{
apsim(exe, wd)
}
\arguments{
  \item{exe}{The path to the APSIM executable file}

  \item{wd}{The working directory containing the .apsim
  files to be run}
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
exe <-" \\"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\\" "
wd <- "C:/Users/Sta36z/Documents/APSIM"
results <- apsimr(exe, wd)
}

