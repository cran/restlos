\name{pMST}
\alias{pMST}

\title{
pMST (minimum spanning tree pruning)
}

\description{
The function determines a robust subsample and computes estimates of location and scatter on the subset.
}

\usage{
pMST(data, N = floor((dim(data)[1] + dim(data)[2] + 1)/2), lmax = dim(data)[1] * 100)
}

\arguments{
  \item{data}{
data set to be analyzed, at least a 2-dimensional matrix whose number of rows (i.e. observations \code{n}) is greater then the number of columns (i.e. dimension \code{d})
}
  \item{N}{
Size of the (robust) subsample to be determined. Default is \code{(n+d+1)/2}
}
  \item{lmax}{
Numerical option: determines the maximal number pruning steps, see deteils.
}
}

\details{
The function uses the \code{mstree} function from the \pkg{ade4}-package to determine the minimum spanning tree (MST) of the data. The resulting MST is iteratively pruned by deleting edges (starting with the longest edge in the MST) until a connected subset with sufficient size (\code{N}) remains. Based on the robust subsample location and scatter are estimated.
}

\value{
 \item{loc}{Location estimate based on the robust subsample}
 \item{cov}{Covariance estimate based on the robust subsample}
 \item{sample}{Index of the observations in the robust subsample}
\item{data}{The input data set}
}

\references{
Kirschstein, T., Liebscher, S., and Becker, C. (201x): Robust estimation of location and scatter by pruning the minimum spanning tree, \emph{under revision}.
}

\author{
Thomas Kirschstein <thomas.kirschstein@wiwi.uni-halle.de>
}

\examples{
## Determine subsample of minimal size
#sub<-pMST(halle)
## Determine subsample of size=900 
#extsub<-pMST(halle, N=900)
}

\keyword{ robust }
