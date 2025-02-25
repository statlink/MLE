\name{MLE of continuous univariate distributions defined on the real line}
\alias{real.mle}
\title{
MLE of continuous univariate distributions defined on the real line}

\description{
MLE of continuous univariate distributions defined on the real line.
}

\usage{
real.mle(x, distr = "normal", v = 5, tol = 1e-7)
}

\arguments{
\item{x}{
A numerical vector with data.
}
\item{distr}{
The distribution to fit, "normal" stands for the normal distribution, "gumbel" for the Gumbel, "cauchy" for the Cauchy, "logistic" for the logistic distribution, "ct" for the (central) t distribution, "t" for the (non-central) t distribution, "wigner" is the Wigner semicircle distribution and "laplace" is the Laplace distribution. "cauchy0" and "gnormal0" are the Cauchy and generalised normal distributions, respectively, with zero location. The generalised normal distribution is also known as the exponential power distribution or the generalized error distribution.
}
\item{v}{
The degrees of freedom of the t distribution.
}
\item{tol}{
The tolerance level up to which the maximisation stops set to 1e-07 by default.
}
}

\details{
Instead of maximising the log-likelihood via a numerical optimiser we have used a Newton-Raphson algorithm which is faster. See wikipedia for the equation to be solved. For the t distribution we need the degrees of freedom and estimate the location and scatter parameters.

The Cauchy is the t distribution with 1 degree of freedom. The Laplace distribution is also called
double exponential distribution.
}

\value{
Usually a list with three elements, but this is not for all cases.
\item{iters}{
The number of iterations required for the Newton-Raphson to converge.
}
\item{scale}{
The estimated scale parameter of the Cauchy distribution.
}
\item{loglik}{
The value of the maximised log-likelihood.
}
\item{param}{
The vector of the parameters.
}
}

\references{
N.L. Johnson, S. Kotz and N. Balakrishnan (1994). Continuous Univariate Distributions, Volume 1 (2nd Edition).

N.L. Johnson, S. Kotz a nd N. Balakrishnan (1970). Distributions in statistics: continuous univariate
distributions, Volume 2.

https://en.wikipedia.org/wiki/Wigner_semicircle_distribution

Do M.N. and Vetterli M. (2002). Wavelet-based Texture Retrieval Using Generalised Gaussian Density
and Kullback-Leibler Distance. Transaction on Image Processing. 11(2): 146--158.
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
