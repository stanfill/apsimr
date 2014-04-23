\name{mm1991mod}
\alias{mm1991mod}
\title{Implement a modified Morris Method}
\usage{
mm1991mod(delta, k)
}
\arguments{
  \item{delta}{step size}

  \item{k}{number of parameters}
}
\value{
list of \itemize{ \item \code{order} The order in which the
variables are changed \item \code{Bstar} The random
permutation matrix }
}
\description{
This function will generate a random orientation matrix
(B*) that is used to implement the modified Morris method.
This method is modified in that the values are chosen
uniformly on the interval [0,1] and delta is set explicitly
by the user
}
\examples{
mm1991mod(.1,2)
}

