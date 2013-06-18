\name{flood}
\alias{flood}

\title{
The Flood Algorithm
}

\description{
The function determines a robust subsample utilizing self-organizing maps (SOM).
}

\usage{
flood(data, Nx=10, Ny=10, rlen=2000)
}

\arguments{
  \item{data}{
At least a two-dimensional data matrix is required. Number of observations needs to be greater than number of dimensions.
}
  \item{Nx}{
Size of the SOM-net in x direction. Default is 10.
}
  \item{Ny}{
Size of the SOM-net in y direction. Default is 10.
}
  \item{rlen}{
Number of iterations during SOM learn process. Default is 2000.
}
}

\details{
The function first calls the \code{som} function within the \pkg{som}-package. The results are subsequently used to determine a robust subsample. Arguments \code{Nx}, \code{Ny} and \code{rlen} are passed to \code{som}. These arguments should be selected depending on the size of the data set (number of observations/dimensions). The larger the data set the larger the net size and the number of iterations should be. Note: At the moment only rectangular and quadratic SOM nets are supported.
}

\value{
  \item{som.results}{SOM results as delivered by \code{som}.}
  \item{som.neigh}{A matrix showing for every neuron (first column) the index off the neighboring neurons (columns 2-5).}
  \item{umatrix}{The U-matrix shows the U-value for every neuron.}
  \item{winneuron}{Vector of length \code{n} giving the index of the nearest neuron (Euclidean distance).}
  \item{lib}{List of all basins found. Index of neurons. Smallest subsample of size \code{(n+d+1)/2}.}
  \item{lin}{List of all neighboring neurons per basin. Index of neurons. Smallest subsample of size \code{(n+d+1)/2}.}
  \item{geb}{Number of associated data points per basin. Smallest subsample of size \code{(n+d+1)/2}.}
  \item{l}{Internal value necessary for plotting.}
  \item{fafh}{Data for plotting the flood area flood height curve.}
  \item{fafh.lib}{Internal data necessary for plotting extented flooding.}
  \item{fafh.drin}{Internal data necessary for plotting extented flooding.}
  \item{drin}{Robust subsample of minimal size.}
}

\references{
Liebscher, S., Kirschstein, T., and Becker, C. (2012): The Flood Algorithm - A Multivariate, Self-Organizing-Map-Based, Robust Location and Covariance Estimator, \emph{Statistics and Computing}, 22(1), 325-336, DOI: 10.1007/s11222-011-9250-3.
}

\author{
Steffen Liebscher <steffen.liebscher@wiwi.uni-halle.de>
}

\examples{
#flood(halle)
}

\keyword{ robust }
