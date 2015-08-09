\name{MVCH}
\alias{MVCH}

\title{
The MVCH Algorithm
}

\description{
The function determines the multivariate mode by iteratively selecting minimum volume convex subsets.
}

\usage{
MVCH(data, ps=0.75, pf=0.2, k=1000, a.poi=2, del.poi=1)
}

\arguments{
  \item{data}{
At least a two-dimensional data matrix is required. Number of observations needs to be greater than number of dimensions.
}
  \item{ps}{
A numeric value between 0 and 1. Fraction of points to be retained in each iteration. Default is set to \code{0.7}. See Details for more information. 
}
\item{pf}{
A numeric value between 0 and 1. Fraction of points determining the size of the final subset. Default is set to \code{0.2}. See Details for more information. 
}
\item{k}{
The maximum number of iterations. Default is set to \code{1000}. See Details for more information.
}
\item{a.poi}{
An integer \eqn{a.poi \ge 1}. Number of points added when searching for minimum volume subsets. Default is set to \code{2}. See Details for more information. 
}
\item{del.poi}{
An integer \eqn{1 \le del.poi < a.poi}. Number of points deleted when searching for minimum volume subsets. Default is set to \code{1}. See Details for more information. 
}
}

\details{

The algorithm iteratively determines a sequence of subsets of certain size with minimum convex hull volume (i.e. minimum volume subsets) until a certain threshold is reached. In the first iteration a minimum volume subset of size \eqn{n_1 = \left\lfloor n \cdot ps\right\rfloor}{n_1=floor(n*ps)} is sought. In the second iteration, out of the subset found in iteration 1, a subset of size \eqn{n_2 = \left\lfloor n_1 \cdot ps\right\rfloor}{n_2=floor(n_1*ps)} is determined. The procedure continues until the threshold is reached: \eqn{\left\lceil n \cdot pf \right \rceil}{ceil(n*pf)} where \code{n} is the number of observations in \code{data}. The mode is calculated as the arithmetic mean of the observations in the final subset. Hence, the combination of \code{ps} and \code{pf} determines the running time and robustness of the procedure. Highest robustness (in terms of maximum breakdown point) is achieved for \eqn{ps = \left \lfloor \frac{n + d + 1}{2} \right \rfloor}{ps=floor((n+d+1)/2)}. Small values of \code{pf}  guarantee an accurate mode estimation also for asymmetric data sets but running times increase.

To find a minimum volume subset, in each iteration \code{in.subs} atomic subsets (consisting of \code{d+1} observations) are constructed. Each of these atomic subsets is iteratively expanded by adding the \code{a.poi} closest points and deleting \code{del.poi}. All three values determine the accuracy of the subset identification (and, hence, the estimate) as well as the running time of the algorithm. Small values of \code{in.subs} reduce running time. Choosing similar values for \code{a.poi} and \code{del.poi} increases running time and algorithm accuracy. 

For more details on the algorithm see the reference. 
}

\value{
A list with following entries:
\item{mode}{The mode estimate.}
\item{set}{The final subset used for mode calculation.}
\item{vol}{The convex hull volume of the final subset.}
\item{set.1}{The subset identified after the first iteration (outlier-free subset).}
}

\references{
Kirschstein, T., Liebscher, S., Porzio, G., Ragozini, G. (2015): Minimum volume peeling: a robust non-parametric estimator of the multivariate mode, \emph{Computational Statistics and Data Analysis}, DOI: 10.1016/j.csda.2015.04.012.
}

\author{
Thomas Kirschstein <thomas.kirschstein@wiwi.uni-halle.de>
}

\examples{
# maximum breakdown point estimation
# MVCH(halle, ps = floor((nrow(halle) + ncol(halle) + 1)/2), pf = 0.05)

# slower estimation
# MVCH(halle, ps = 0.75, pf = 0.05)

# quicker estimation
# MVCH(halle, ps = 0.25, pf = 0.05)

}

\keyword{ robust }
