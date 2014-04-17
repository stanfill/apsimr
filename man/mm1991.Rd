\name{mm1991}
\alias{mm1991}
\title{Implement the Morris Method (1991)}
\usage{
mm1991(p, k)
}
\arguments{
  \item{p}{controls delta, step size}

  \item{k}{number of parameters}
}
\value{
list of \itemize{ \item \code{order} The order in which the
variables are changed \item \code{Bstar} The random
permutation matrix }
}
\description{
This function will generate a random orientation matrix
(B*) that is used to implement the Morris method.
}
\examples{
mm1991(10,2)
}

