\name{apsimr-package}
\alias{apsimr-package}
\alias{apsimr}
\docType{package}
\title{
Run APSIM from R
apsimr
}
\description{
Allows for the running of APSIM from R.  The anslysis and visualization of simulation results is also possible.
}
\details{
\tabular{ll}{
Package: \tab apsimr\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-03-14\cr
License: \tab What license is it under?\cr
}
the function runAPSIM will call APSIM and run the selected files in the provided directory
}
\author{
Bryan Stanfill

Maintainer: Bryan Stanfill <bryan.stanfill@csiro.au>
}
\references{
See http://www.apsim.info/
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the ~~
%~~ R documentation directory ~~
\keyword{ package }
\seealso{
 \code{\link[APSIMBatch:runAPSIM]{APSIMBatch}}
}
\examples{
exe <-" \"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\" "
wd <- "C:/Users/Sta36z/Documents/APSIM"
results <- apsimr(exe, wd)
}
