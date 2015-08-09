\name{rdela}
\alias{rdela}

\title{
The RDELA Algorithm
}

\description{
The function determines a robust subsample utilizing the Delaunay triangulation.
}

\usage{
rdela(data, N, rew=TRUE)
}
\arguments{
  \item{data}{
At least a two-dimensional data matrix is required. Number of observations needs to be greater than the number of dimensions. No degenerated (i.e. collinear) data sets allowed.
}
  \item{N}{
Size of the identified subsample. Default is \code{(n+d+1)/2}.
}
  \item{rew}{
Logical. Specifies whether reweighting should be conducted (TRUE) or not (FALSE). Default is TRUE.
}
}

\details{
The function first calls the \code{delaunayn} function within the \pkg{geometry}-package. The results are subsequently used to determine a robust subsample.
}

\value{
  \item{data}{The input data set.}
  \item{tri}{Vertices of all simplices of the Delaunay triangulation. Each row represents a simplex.}
  \item{neigh}{Lists for every simplex the adjacent/neighboring simplices. Each list entry represents a simplex.}
  \item{radii}{Circum-(hypersphere-)radius of each simplex.}
  \item{center}{Center coordinates of all simplices.}
  \item{LiB}{List of all basins found. Index of simplices. Smallest subsample of size \code{(n+d+1)/2}.}
  \item{LiN}{List of all neighboring simplices per basin. Index of simplices. Smallest subsample of size \code{(n+d+1)/2}.}
  \item{GeB}{Number of associated data points per basin. Smallest subsample of size \code{(n+d+1)/2}.}
  \item{drin}{(Initial) Robust subsample of size N.}
  \item{raw.mean}{Mean estimate based on (initial) robust subsample of size N.}
  \item{raw.cov}{Covariance estimate based on (initial) robust subsample of size N.}
  \item{final}{Final robust subsample after reweighting.}
  \item{mean}{Mean estimate based on final robust subsample.}
  \item{cov}{Covariance estimate based on final robust subsample.}
}

\references{
Liebscher, S., Kirschstein, T. (2015): Efficiency of the pMST and RDELA Location and Scatter Estimators, \emph{AStA-Advances in Statistical Analysis}, 99(1), 63-82, DOI: 10.1007/s10182-014-0231-7.

Liebscher, S., Kirschstein, T., and Becker, C. (2013): RDELA - A Delaunay-Triangulation-based, Location and Covariance Estimator with High Breakdown Point, \emph{Statistics and Computing}, DOI: 10.1007/s11222-012-9337-5.
}

\author{
Steffen Liebscher <steffen.liebscher@wiwi.uni-halle.de>
}

\examples{
# rdela(halle)
}

\keyword{ robust }
