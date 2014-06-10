\name{plot.apsim}
\alias{plot.apsim}
\title{Visualize and APSIM run}
\usage{
\method{plot}{apsim}(x, y = NULL, ask = TRUE, ...)
}
\arguments{
  \item{x}{Data frame of class 'apsim' including the
  results of an APSIM simulation}

  \item{y}{variable to plot on y-axis.  If left empty all
  variables will be plotted on seperate plots.}

  \item{ask}{logical; if \code{TRUE}, the user is asked
  before ach plot, see \code{\link{par}}(ask=.)}

  \item{...}{additional arguments passed to
  \code{\link[ggplot2:qplot]{qplot}}}
}
\value{
nothing is returned
}
\description{
Visualize and APSIM run
}
\examples{
\dontrun{
exe <-" \\"C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe\\" "
wd <- "../APSIM"
toRun <- c("Centro.apsim","Continuous Wheat.apsim")
results <- apsim(exe, wd, files = toRun)

#Look at all of the results as a function of time
plot(results[[2]])

#Plot just yield as a function of time
plot(results[[2]],y='yield')+geom_line(colour='red')+theme_bw()
}
}

