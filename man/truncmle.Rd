\name{MLE of some truncated distributions}
\alias{truncmle}

\title{MLE of some truncated distributions
}

\description{
MLE of some truncated distributions.
}

\usage{
truncmle(x, distr = "trunccauchy", a, b, tol = 1e-07)
}

\arguments{
\item{x}{
A numerical vector with continuous data. For the Cauchy distribnution, they can be anywhere on the real line. For the exponential distribution they must be strictly positive.
}
\item{distr}{
The type of distribution to fit, "trunccauchy" and "truncexpmle" stand for the truncated Cauchy and truncated exponential distributions, respectively.
}
\item{a}{
The lower value at which the Cauchy distribution is truncated.
}
\item{b}{
The upper value at which the Cauchy or the exponential distribution is truncated. For the exponential this must
be greater than zero.
}
\item{tol}{
The tolerance value to terminate the fitting algorithm.
}
}

\details{
Maximum likelihood of some truncated distributions is performed.
}

\value{
A list including:
\item{iters}{
The number of iterations reuired by the Newton-Raphson algorithm.
}
\item{loglik}{
The log-likelihood.
}
\item{lambda}{
The \eqn{\lambda} parameter in the exponential distribution.
}
\item{param}{
The location and scale parameters in the Cauchy distribution.
}
}

\references{
David Olive (2018). Applied Robust Statistics (Chapter 4).

http://lagrange.math.siu.edu/Olive/ol-bookp.htm
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

\seealso{
\code{ \link{cens.mle}
}
}

\examples{
x <- rnorm(500)
truncmle(x, a = -1, b = 1)
}

