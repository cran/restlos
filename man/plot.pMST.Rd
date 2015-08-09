\name{plot.pMST}
\alias{plot.pMST}

\title{
Plot function for objects of class \code{pMST}
}

\description{
Function to plot the results obtained by function \code{pMST}.
}

\usage{
\method{plot}{pMST}(x, ...)
}

\arguments{
  \item{x}{
Object of class \code{pMST}.
}
  \item{...}{
Further graphical parameters.
}
}

\details{
The resulting plots display the LC- and the AL-plot to support the decision on the size of the robust subsample, see references. Moreover, if the data set has dimension 2 or 3, the data set is plotted with the chosen robust subset  superimposed as red points.
}

\references{
Kirschstein, T., Liebscher, S., and Becker, C. (2013): Robust estimation of location and scatter by pruning the minimum spanning tree, \emph{Journal of Multivariate Analysis}, 120, 173-184, DOI: 10.1016/j.jmva.2013.05.004.
}

\author{
Thomas Kirschstein <thomas.kirschstein@wiwi.uni-halle.de>
}

\examples{
# plot(pMST(halle))
}

\keyword{ robust }
