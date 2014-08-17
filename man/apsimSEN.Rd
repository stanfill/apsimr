\name{apsimSEN}
\alias{apsimSEN}
\title{Edit and run an APSIM simulation}
\usage{
  apsimSEN(X, exe, wd, vars, toRun, toEdit = toRun,
    overwrite = FALSE, g)
}
\arguments{
  \item{X}{N-by-p matrix of inputs with rows corresponding
  to runs and columns for variables}

  \item{exe}{where to find the APSIM executable}

  \item{wd}{directory where the .apsim file lives and where
  the results will be saved}

  \item{vars}{names of the variables, must be of the same
  length as 'X' has columns}

  \item{toRun}{the .apsim file in \code{wd} to run}

  \item{toEdit}{the .apsim file or .xml file to be edited}

  \item{g}{a function of the output returned by apsim -
  must give univariate result}
}
\value{
  a vector of univariate responses
}
\description{
  This function will both edit and run the edited APSIM
  simulation.  This function is primarily for use with the
  sensitivity package but it can be useful on its own as
  well.
}
\examples{
\dontrun{
g<-function(X){
 return(mean(X$lai_cowpea))
}

wd <- "~/APSIM"
var <- c(rep("SoilWater/Thickness",11), "SoilOrganicMatter/SoilCN")
value <- matrix(c(rep(200, 2), rep(300, 9), 10,rep(250, 2), rep(350, 9), 12),nrow=2,byrow=T)
exe <-"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
file <- "Canopy.apsim"

res <- apsimSEN(X=value,exe=exe, wd=wd,vars=var,toRun=file,g=g)
}
}

