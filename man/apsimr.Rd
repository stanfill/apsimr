\name{apsimr}
\alias{apsimr}
\title{Connect to APSIM}
\usage{
apsimr(exe, wd)
}
\arguments{
  \item{exe}{The path to the APSIM executable file}

  \item{wd}{The working directory containing the .apsim
  files to be run}
}
\description{
This function will call APSIM from R.
}
\examples{
exe <-" \\"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\\" "
wd <- "C:/Users/Sta36z/Documents/APSIM"
results <- apsimr(exe, wd)
}

