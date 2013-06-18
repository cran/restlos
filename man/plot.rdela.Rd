\name{plot.rdela}
\alias{plot.rdela}

\title{
Plot function for objects of class \code{rdela}
}

\description{
Function to plot the results obtained by function \code{rdela}
}

\usage{
\method{plot}{rdela}(x,...)
}

\arguments{
  \item{x}{
Object of class \code{rdela}.
}
  \item{...}{
Further graphical parameters.
}
}

\details{
The resulting plots depend on the dimensionality of the data set. For \code{d=2} and \code{d=3} the data set and the selected robust subsample are plotted. For \code{d>3} a Mahalanobis distance plot is generated instead.
}

\references{
Liebscher, S., Kirschstein, T., and Becker, C. (2013): RDELA - A Delaunay-Triangulation-based, Location and Covariance Estimator with High Breakdown Point, \emph{Statistics and Computing}, DOI: 10.1007/s11222-012-9337-5.
}

\author{
Steffen Liebscher <steffen.liebscher@wiwi.uni-halle.de>
}

\note{
At the moment no additional graphical parameters can be passed.
}

\examples{
#plot(rdela(halle))
}

\keyword{ robust }
