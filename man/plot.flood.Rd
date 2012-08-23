\name{plot.flood}
\alias{plot.flood}

\title{
Plot function for objects of class \code{flood}
}

\description{
Function to plot the results obtained by function \code{flood}
}

\usage{
\method{plot}{flood}(x,..., level = 0)
}

\arguments{
  \item{x}{
Object of class \code{flood}.
}
  \item{level}{
Flood level. Numeric value between 0 and 1. Default is 0 (i.e. all plots are based on the smallest robust subsample).}
  \item{...}{
Further graphical parameters.
}
}

\details{
The resulting plots depend on the dimensionality of the data set. For \code{d=2} and \code{d=3} the data set and the superimposed SOM net are plotted. For \code{d>3} a Mahalanobis distance plot is generated instead. The U-landscape and the Flood-Area-Flood-Height-curve are always plotted.
}

\references{
Liebscher, S., Kirschstein, T., and Becker, C. (2012): The Flood Algorithm - A Multivariate, Self-Organizing-Map-Based, Robust Location and Covariance Estimator, \emph{Statistics and Computing}, DOI: 10.1007/s11222-011-9250-3.
}

\author{
Steffen Liebscher <steffen.liebscher@wiwi.uni-halle.de>
}

\note{
At the moment no additional graphical parameters can be passed.
}

\examples{
#plot(flood(halle))
}

\keyword{ robust }
