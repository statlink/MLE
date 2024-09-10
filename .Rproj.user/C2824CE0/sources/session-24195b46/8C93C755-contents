\name{Column-wise MLE of continuous univariate distributions defined on the real line}
\alias{colreal.mle}
\title{
Column-wise MLE of continuous univariate distributions defined on the real line}

\description{
Column-wise MLE of continuous univariate distributions defined on the real line.
}

\usage{
colreal.mle(x, distr = "normal", tol = 1e-07, maxiters = 100, parallel = FALSE)
}

\arguments{
\item{x}{
A numerical vector with data.
}
\item{distr}{
The distribution to fit, "normal" stands for the normal distribution, "cauchy" for the Cauchy,  "laplace" is the Laplace distribution.
}
\item{tol}{
The tolerance level to stop the iterative process of finding the MLEs.
}
\item{maxiters}{
The maximum number of iterations to implement.
}
\item{parallel}{
Should the computations take place in parallel?
}
}

\details{
Instead of maximising the log-likelihood via a numerical optimiser we have used a Newton-Raphson algorithm which is faster. See wikipedia for the equation to be solved. For the t distribution we need the degrees of freedom and estimate the location and scatter parameters.

The Cauchy is the t distribution with 1 degree of freedom. The Laplace distribution is also called
double exponential distribution.
}

\value{
A matrix with two, columns. The first one contains the parameters of the distribution
and the second columns contains the log-likelihood values.
}

\references{
Johnson, Norman L. Kemp, Adrianne W. Kotz, Samuel (2005). Univariate Discrete Distributions (third edition). Hoboken, NJ: Wiley-Interscience.

https://en.wikipedia.org/wiki/Wigner_semicircle_distribution
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

\seealso{
\code{ \link{positive.mle}, \link{circ.mle}, \link{disc.mle}
}
}

\examples{
x <- rnorm(1000, 10, 2)
a <- real.mle(x, distr = "normal")
}
