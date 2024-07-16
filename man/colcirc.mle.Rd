\name{Column-wise MLE of some circular distributions}
\alias{colcirc.mle}
\title{
Column-wise MLE of some circular distributions
}

\description{
Column-wise MLE of some circular distributions.
}

\usage{
colcirc.mle(x, distr = "vm", tol = 1e-07, maxiters = 100, parallel = FALSE)
}

\arguments{
\item{x}{
A numerical matrix with the circular data. They must be expressed in radians.
}
\item{distr}{
The type of distribution to fit, "vm" stands for the von Mises and "spml" is the angular Gaussian distribution.
}
\item{tol}{
The tolerance level to stop the iterative process of finding the MLEs.
}
\item{maxiters}{
The maximum number of iterations to implement. This is for the "spml" only.
}
\item{parallel}{
Should the computations take place in parallel? This is for the "spml" only.
}
}

\details{
The parameters of the von Mises, the bivariate angular Gaussian and wrapped Cauchy distributions are estimated.
For the wrapped Cauchy, the iterative procedure described by Kent and Tyler (1988) is used.
As for the von Mises distribution, we use a Newton-Raphson to estimate the concentration parameter.
The angular Gaussian is described, in the regression setting in Presnell et al. (1998).
}

\value{
A matrix with two, columns. The first one contains the parameters of the distribution
and the second columns contains the log-likelihood values.
}

\references{
Mardia K. V. and Jupp P. E. (2000). Directional statistics. Chicester: John Wiley & Sons.

Sra S. (2012). A short note on parameter approximation for von Mises-Fisher distributions:
and a fast implementation of Is(x). Computational Statistics, 27(1): 177-190.

Presnell Brett, Morrison Scott P. and Littell Ramon C. (1998). Projected multivariate linear models for directional data.
Journal of the American Statistical Association, 93(443): 1068--1077.

Kent J. and Tyler D. (1988). Maximum likelihood estimation for the wrapped Cauchy distribution.
Journal of Applied Statistics, 15(2): 247--254.
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{circ.mle},
}
}

\examples{
x <- matrix( rnorm(100 * 10, 3, 1), ncol = 10)
x <- x / sqrt( rowSums(x^2) )
res <- colcirc.mle(x, distr = "spml")
}

