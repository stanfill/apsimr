\name{edit_sim_file}
\alias{edit_sim_file}
\title{Edit an APSIM plugin file}
\usage{
edit_sim_file(file, var, value, overwrite = FALSE)
}
\arguments{
  \item{file}{file ending in ".xml" to be edits}

  \item{var}{vector of variables to be edited}

  \item{value}{list of new values for the defined
  variables}

  \item{overwrite}{logical; if \code{TRUE} the old file is
  overwritten, a new file is written otherwise}
}
\value{
if the file is successfully written then nothing is
returned
}
\description{
APSIM uses .xml files to dictate how cretain processes are
carried out.  Similar to \code{\link{edit_apsim}} this
function edits a file that will be used in an APSIM
simulation.  Unlike \code{\link{edit_apsim}} this function
edits the .xml simulation files. The variables specified by
\code{var} within the .xml file specified by \code{file} in
the current working directory is edited. The old values are
replaced with \code{value}, which is a list that has the
same number of objects are the length of \code{var}.  The
current .xml file will be overwritten if \code{overwrite}
is set to \code{TRUE}; otherwise the file
\emph{file-edited.xml} will be created.
}
\examples{
\dontrun{
wd <- "C:/Users/Sta36z/Documents/APSIM"
setwd(wd)
#The file I want to edit is called "Soil.xml"
file <- "Soil.xml"

#I want to change the potential nitrification and N2O from nitrification
var <- c("nitrification_pot", "dnit_nitrf_loss")

#Change both to absolute values of random N(0,1)
value <- list(abs(rnorm(1)), abs(rnorm(1)))

#Edit the apsim file without overwriting it
edit_sim_file(file, var, value, overwrite = FALSE)
}
}

