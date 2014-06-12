\name{apsimr-package}
\alias{apsimr-package}
\alias{apsimr}
\docType{package}
\title{
Run APSIM from R
}
\description{
The \bold{A}gricultural \bold{P}roduction \bold{S}ystems s\bold{IM}ulator (APSIM) is a widely used simulator of agricultural systems.  This package includes a few basic functions to create, edit and run APSIM simulations then visualize the results.  An installation of APSIM is required for this package to be of any use.  Assuming non-commercial use, APSIM can be downloaded for free from \url{http://www.apsim.info/Products/Downloads.aspx}.
}
\details{
\tabular{ll}{
Package: \tab apsimr\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-06-10\cr
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
 \code{\link[APSIMBatch:runAPSIM]{APSIMBatch}}
}
\examples{
\dontrun{
exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
wd <- "../APSIM"
results <- apsim(exe, wd, "Centro.apsim")
plot(results)
}
}
