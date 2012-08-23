\name{rdela}
\alias{rdela}

\title{
The RDELA Algorithm
}

\description{
The function determines a robust subsample utilizing the Delaunay triangulation.
}

\usage{
rdela(data, N)
}
\arguments{
  \item{data}{
At least a two-dimensional data matrix is required. Number of observations need to be greater than number of dimensions. No degenerated (i.e. collinear) data sets allowed.
}
  \item{N}{
Size of the identified subsample. Default is \code{(n+d+1)/2}.
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
  \item{drin}{Robust subsample of minimal size.}
}

\references{
Liebscher, S., Kirschstein, T., and Becker, C. (201x): RDELA - A Delaunay-Triangulation-based, Location and Covariance Estimator with High Breakdown Point, to appear in \emph{Statistics and Computing}, DOI: 10.1007/s11222-012-9337-5.
}

\author{
Steffen Liebscher <steffen.liebscher@wiwi.uni-halle.de>
}

\examples{
#rdela(halle)
}

\keyword{ robust }
