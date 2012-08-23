\name{plot.pMST}
\alias{plot.pMST}

\title{
Plot function for objects of class \code{pMST}
}

\description{
Function to plot the results obtained by the functions \code{pMST}.
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
The resulting plots display the LC- and the AL-plot to support the decision on the size of the robust subsample, see references. Moreover, if data set has dimension 2 or 3 the data set is plotted where the chosen robust subset is superimposed as red points.
}

\references{
Kirschstein, T., Liebscher, S., and Becker, C. (201x): Robust estimation of location and scatter by pruning the minimum spanning tree, \emph{under revision}.
}

\author{
Thomas Kirschstein <thomas.kirschstein@wiwi.uni-halle.de>
}

\examples{
#plot(pMST(halle))
}

\keyword{ robust }
