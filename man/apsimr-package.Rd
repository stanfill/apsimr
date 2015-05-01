\name{apsimr-package}
\alias{apsimr-package}
\alias{apsimr}
\docType{package}
\title{
Run, edit, visualize APSIM from R
}
\description{
The \bold{A}gricultural \bold{P}roduction \bold{S}ystems s\bold{IM}ulator (APSIM) is a widely used simulator of agricultural systems.  This package includes a few basic functions to create, edit and run APSIM simulations from R.  It also includes functions to visualize the results of an APSIM simulation and perform sensitivity/uncertainty analysis of APSIM either via functions in the sensitivity package or by included emulator-based sensitivity analysis function.  An installation of APSIM is required for this package to be of any use.  Assuming non-commercial use, APSIM can be downloaded for free from \url{http://www.apsim.info/Products/Downloads.aspx}.
}
\details{
\tabular{ll}{
Package: \tab apsimr\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-01\cr
License: \tab GPL(>=3) \cr
}
}
\author{
Bryan Stanfill

Maintainer: Bryan Stanfill <bryan.stanfill@csiro.au>
}
\references{
See \url{http://www.apsim.info/} for information about APSIM.
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the ~~
%~~ R documentation directory ~~
\keyword{ package }
\seealso{
 \code{\link[APSIMBatch:runAPSIM]{APSIMBatch}}, 
 \code{\link[sensitivity]{sensitivity}}
}
\examples{
\dontrun{
apsimExe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
apsimWd <- "~/APSIM"
apsimResults <- apsim(apsimExe, apsimWd, "Centro.apsim")
plot(apsimResults)
}
}
