\name{Column-wise MLE of continuous univariate distributions defined on the positive line}
\alias{colpositive.mle}
\title{
Column-wise MLE of continuous univariate distributions defined on the positive line}

\description{
Column-wise MLE of continuous univariate distributions defined on the positive line.
}

\usage{
colpositive.mle(x, distr = "gamma", tol = 1e-07, maxiters = 100, parallel = FALSE)
}

\arguments{
\item{x}{
A matrix with positive valued data (zeros are not allowed).
}
\item{distr}{
The distribution to fit. "gamma" stands for the gamma distribution, "weibull" for the Weibull, "pareto" for the Pareto distribution, "exp" for the exponential distribution, "exp2" I do not remember, "maxboltz" for the Maxwell-Boltzman distribution, "rayleigh" for the Rayleigh distribution and "lindley" for the Lindley distribution, "lognorm" for the log-normal distribution. "halfnorm" for the half-normal, "invgauss" for the inverse Gaussian. The "normlog" is simply the normal distribution where all values are positive. Note, this is not log-normal. It is the normal with a log link. Similarly to the inverse gaussian distribution where the mean is an exponentiated. This comes from the GLM theory. The "powerlaw" stands for the power law distribution.
}
\item{tol}{
The tolerance level up to which the maximisation stops; set to 1e-07 by default.
}
\item{maxiters}{
The maximum number of iterations the Newton-Raphson will perform for the Weibull distribution.
}
\item{parallel}{
Do you want to calculations to take place in parallel? The default value is FALSE. This is only for the Weibull distribution.
}
}

\details{
For each column, the same distribution is fitted and its parameter and log-likelihood are computed.
}

\value{
A matrix with two, three or five (for the colnormlog.mle) columns. The first one or the first two contain the parameter(s) of the distribution and the other columns contain the log-likelihood values.
}

\references{
Kalimuthu Krishnamoorthy, Meesook Lee and Wang Xiao (2015). Likelihood ratio tests for comparing several
gamma distributions. Environmetrics, 26(8): 571--583.

N.L. Johnson, S. Kotz and N. Balakrishnan (1994). Continuous Univariate Distributions, Volume 1 (2nd Edition).

N.L. Johnson, S. Kotz a nd N. Balakrishnan (1970). Distributions in statistics: continuous univariate
distributions, Volume 2.

Tsagris M., Beneki C. and Hassani H. (2014). On the folded normal distribution. Mathematics, 2(1): 12--28.

Sharma V. K., Singh S. K., Singh U. and Agiwal V. (2015). The inverse Lindley distribution:
a stress-strength reliability model with application to head and neck cancer data. Journal of
Industrial and Production Engineering, 32(3): 162--173.

You can also check the relevant wikipedia pages for these distributions.
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

\seealso{
\code{ \link{disc.mle}, \link{real.mle}, \link{prop.mle}
}
}

\examples{
x <- rgamma(100, 3, 4)
positive.mle(x, distr = "gamma")
}
