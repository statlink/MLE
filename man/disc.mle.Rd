\name{MLE of count data (univariate discrete distributions)}
\alias{disc.mle}
\title{
MLE of count data
}

\description{
MLE of count data.
}

\usage{
disc.mle(x, distr = "poisson", N = NULL, type = 1, tol = 1e-07)
}

\arguments{
\item{x}{
A vector with discrete valued data.
}
\item{distr}{
The distribution to fit, "poisson" stands for the Poisson, "zip" for the zero-inflated Poisson, "ztp" for the zero-truncated Poisson, "negbin" for the negative binomial, "binom" for the binomial, "borel" for the Borel distribution, "geom" for the geometric, "logseries" for the log-series distribution, "betageom" for the beta-geometric, "betabinom" for the beta-binomial distribution and "skellam" for the Skellam distribution, "gp" for the generalised Poisson distribution and "gammapois" for the gamma-Poisson distribution.
}
\item{type}{
This argument is for the negative binomial and the geometric distribution.
In the negative binomial you can choose which way your prefer. Type 1 is for smal sample sizes, whereas
type 2 is for larger ones as is faster. For the geometric it is related to its two forms. Type 1 refers to the case where the minimum is zero and type 2 for the case of the minimum being 1.
}
\item{N}{
This is for the binomial distribution only, specifying the total number of successes. If NULL, it is sestimated by the data. It can also be a vector of successes.
}
\item{tol}{
The tolerance level up to which the maximisation stops set to 1e-07 by default.
}
}

\details{
Instead of maximising the log-likelihood via a numerical optimiser we used a Newton-Raphson algorithm which is faster.

See wikipedia for the equation to be solved in the case of the zero inflated distribution. https://en.wikipedia.org/wiki/Zero-inflated_model.
In order to avoid negative values we have used link functions, log for the \eqn{lambda} and logit for the \eqn{\pi} as suggested by Lambert (1992).
As for the zero truncated Poisson see https://en.wikipedia.org/wiki/Zero-truncated_Poisson_distribution.
}

\value{
The following list is not inclusive of all cases. Different functions have different names. In general a list including:
\item{mess}{
This is for the negbin.mle only. If there is no reason to use the negative binomial distribution a message will appear, otherwise this is NULL.
}
\item{iters}{The number of iterations required for the Newton-Raphson to converge.
}
\item{loglik}{The value of the maximised log-likelihood.
}
\item{prob}{The probability parameter of the distribution. In some distributions this argument might have a different name.
For example, param in the zero inflated Poisson.
}
}

\references{
Lambert D. (1992). Zero-Inflated Poisson Regression, with an Application to Defects in
Manufacturing. Technometrics. 34 (1): 1--14

Johnson N. L., Kotz S. and Kemp A. W. (1992). Univariate Discrete
Distributions (2nd ed.). Wiley.

Skellam J. G. (1946) The frequency distribution of the difference between two Poisson variates belonging to different populations.
Journal of the Royal Statistical Society, series A 109/3, 26.

Nikoloulopoulos A.K. and Karlis D. (2008). On modeling count data: a comparison of some well-known discrete distributions.
Journal of Statistical Computation and Simulation, 78(3): 437--457.
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

\seealso{
\code{ \link{real.mle}
}
}

\examples{
x <- rpois(100, 2)
disc.mle(x, type = "poisson")
}
