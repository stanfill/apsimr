\name{plot.apsim}
\alias{plot.apsim}
\title{Visualize the results of an APSIM simulation}
\usage{
  \method{plot}{apsim} (x, y = NULL, ask = TRUE,
    one_plot = FALSE, ...)
}
\arguments{
  \item{x}{data frame of class "apsim" including the
  results of an APSIM simulation}

  \item{y}{variable to plot on y-axis; if left empty all
  variables will be plotted on separate plots}

  \item{ask}{logical; if \code{TRUE}, the user is asked
  before each plot, see \code{\link{par}(ask=.)}}

  \item{one_plot}{logical; if \code{TRUE} all variables are
  plotted on one faceted plot}

  \item{...}{additional arguments passed to
  \code{\link[ggplot2:qplot]{qplot}}}
}
\description{
  Similar to the \code{\link[stats:plot.lm]{plot}} for
  \code{\link{lm}} objects, \code{plot.apsim} will plot
  each response in the results of an APSIM simulation on
  its own \code{ggplot2} object.  If the \code{one_plot}
  argument is set to \code{TRUE} then
  \code{\link[ggplot2:facet_wrap]{facet_wrap}} is used to
  plot all of the responses on one screen.  Or only one
  response can be plotted by setting \code{y} to a column
  name in \code{x}.
}
\examples{
\dontrun{
exe <- "C:/Program Files (x86)/Apsim76-r3376/Model/Apsim.exe"
wd <- "~/APSIM"
toRun <- c("Centro.apsim", "Continuous Wheat.apsim")
results <- apsim(exe, wd, files = toRun)

#Look at all of the results as a function of time in seperate plots
plot(results[[2]])

#Put all variables on one faceted plot
plot(results[[2]], one_plot = TRUE) + theme_bw()

#Plot just yield as a function of time
plot(results[[2]], y = 'yield') + geom_line(colour = 'red') + theme_bw()
}
}

