\name{Column-wise MLE of some discrete distributions}
\alias{coldisc.mle}

\title{
Column-wise MLE of some discrete distributions
}

\description{
Column-wise MLE of some discrete distributions.
}

\usage{
coldisc.mle(x, distr = "poisson", type = 1)
}

\arguments{
\item{x}{
A numerical matrix with count data, dscrete data, integers. Each column refers to a different vector of observations of the same distribution.
}
\item{distr}{
The distribution to fit, "poisson" stands for the Poisson, "geom" for the geometric distribution,
"borel" for the Borel distribution and "gamma" for the Gamma distribution.
}
\item{type}{
This is for the geometric distribution only. Type 1 refers to the case where the
minimum is zero and type 2 for the case of the minimum being 1.
}
}

\details{
For each column, the same distribution is fitted and its parameter and log-likelihood are computed.
}

\value{
A matrix with two, columns. The first one contains the parameters of the distribution
and the second columns contains the log-likelihood values.
}

\references{
Johnson Norman L., Kotz Samuel and Balakrishnan (1997). Discrete Multivariate Distributions. Wiley.
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

\seealso{
\code{\link{disc.mle} }
}

\examples{
x <- matrix(rpois(1000 * 50, 10), ncol = 50)
a <- coldisc.mle(x, distr = "poisson")
}
