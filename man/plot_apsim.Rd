\name{plot_apsim}
\alias{plot_apsim}
\title{Visualize and APSIM run}
\usage{
\method{plot}{apsim}(x, y = NULL, data, ...)
}
\arguments{
  \item{x}{variable to plot on x-axis}

  \item{y}{variable to plot on y-axis}

  \item{data}{Object of class .apsim to be plotted}

  \item{var}{Variable to be visualized}

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
exe <-" \\"C:/Program Files (x86)/Apsim75-r3008/Model/Apsim.exe\\" "
wd <- "C:/Users/Sta36z/Documents/APSIM"
toRun <- c("Centro.apsim","Continuous Wheat.apsim")
results <- apsimr(exe, wd, files = toRun)
plot_apsim(x='Date',data=results[[2]])
}
}

