\name{MLE of some matrix distributions}
\alias{matrix.mle}
\title{
MLE of some matrix distributions
}

\description{
MLE of some matrix distributions some matrix distributions.
}

\usage{
matrix.mle(X, distr = "MN")
}

\arguments{
\item{X}{
For the matrix normal, a list with k elements (k is the sample size), k matrices of dimension
\eqn{n \times p} each. For the matrix Fisher an array containing rotation matrices in SO(3).
}
\item{distr}{
The distribution to fit. "MN" stands for the matrix normal, while "mfisher" stands for the matrix Fisher distribution (defined in SO(3)).
}
}

\value{
For the matrix normal a list including:
\item{runtime}{
The runtime required for the whole fitting procedure.
}
\item{iters}{
The number of iterations required for the estimation of the U and V matrices.
}
\item{M}{
The estimated mean matrix of the distribution, a numerical matrix of dimensions \eqn{n \times p}.
}
\item{U}{
The estimated covariance matrix associated with the rows, a numerical matrix of dimensions \eqn{n \times n}.
}
\item{V}{
The estimated covariance matrix associated with the columns, a numerical matrix of dimensions \eqn{p \times p}.
}
For the matrix Fisher the components of \eqn{ svd( \bar{X} ) }.
}

\references{
Pocuca N., Gallaugher M. P., Clark K. M. and McNicholas P. D. (2019). Assessing and Visualizing Matrix Variate Normality. arXiv:1910.02859.

https://en.wikipedia.org/wiki/Matrix_normal_distribution#Definition

Prentice M. J. (1986). Orientation statistics without parametric assumptions.
Journal of the Royal Statistical Society. Series B (Methodological), 48(2): 214--222.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{\link{mv.mle}, \link{hspher.mle}
}
}

\examples{
## silly example
n <- 8  ;  p <- 4
X <- list()
for ( i in 1:200 )
X[[ i ]] <- matrix( rnorm(n * p), ncol = p )
mod <- matrix.mle(X)
}

\keyword{ Matrix Fisher distribution }
\keyword{ Maximum likelihood estimation }
