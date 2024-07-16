\name{MLE for multivariate discrete data}
\alias{mvdisc.mle}
\title{
MLE for multivariate discrete data
}

\description{
MLE for multivariate discrete data.
}

\usage{
mvdisc.mle(x, distr = "multinom", tol = 1e-07)
}

\arguments{
\item{x}{
A matrix with discrete valued non negative data.
}
\item{distr}{
The distribution to fit. "multinom" stands for the multinomial distribution, "dirimultinom" stands for the Dirichlet-multinomial distribution. "bp.mle" and "bp.mle2" stand for the bivariate Poisson distribution. The The "bp.mle" returns a lot of information and is slower than "bp.mle2", which returns fewer information, but is faster.
}
\item{tol}{
The tolerance level to terminate the Newton-Raphson algorithm for the Dirichlet multinomial distribution.
}
}

\value{
A list including:
\item{iters}{
The number of iterations required by the Newton-Raphson algortihm.
}
\item{loglik}{
A vector with the value of the maximised log-likelihood.
}
\item{param}{
A vector of the parameters.
}
\item{prob}{
A vector with the estimated probabilities.
}

For the "bp.mle" a list including:
\item{lambda}{
A vector with the estimated values of \eqn{(\lambda_1}, \eqn{\lambda_2)} and \eqn{\lambda_3}.
Note that \eqn{\hat{\lambda}_1=\bar{x}_1 - \lambda_3} and \eqn{\hat{\lambda}_1=\bar{x}_1 - \lambda_3}, where
\eqn{\bar{x}_1} and \eqn{\bar{x}_2} are the two sample means.
}
\item{rho}{
The estimated correlation coefficient, that is: \eqn{\dfrac{\hat{\lambda}_3}{\sqrt{\left(\hat{\lambda}_1 + \hat{\lambda_3}\right)\left(\hat{\lambda}_2 + \hat{\lambda_3}\right)}}}.
}
\item{ci}{
The 95\% Confidence intervals using the observed and the asymptotic information matrix.
}
\item{loglik}{
The log-likelihood values assuming independence (\eqn{\lambda_3=0}) and assuming the bivariate Poisson distribution.
}
\item{pvalue}{
Three p-values for testing \eqn{\lambda_3=0}. These are based on the log-likelihood ratio and two Wald tests using the observed and the asymptotic information matrix.
}

For the "bp.mle2" a list including:
\item{lambda}{
A vector with the estimated values of \eqn{(\lambda_1}, \eqn{\lambda_2)} and \eqn{\lambda_3}.
Note that \eqn{\hat{\lambda}_1=\bar{x}_1 - \lambda_3} and \eqn{\hat{\lambda}_1=\bar{x}_1 - \lambda_3}, where
\eqn{\bar{x}_1} and \eqn{\bar{x}_2} are the two sample means.
}
\item{loglik}{
The log-likelihood values assuming independence (\eqn{\lambda_3=0}) and assuming the bivariate Poisson distribution.
}

}

\references{
Johnson Norman L., Kotz Samuel and Balakrishnan (1997). Discrete Multivariate Distributions. Wiley.

Kawamura K. (1984). Direct calculation of maximum likelihood estimator for the bivariate
Poisson distribution. Kodai Mathematical Journal, 7(2): 211--221.

Kocherlakota S. and Kocherlakota K. (1992). Bivariate discrete distributions. CRC Press.

Karlis D. and Ntzoufras I. (2003). Analysis of sports data by using bivariate poisson models.
Journal of the Royal Statistical Society: Series D (The Statistician), 52(3): 381--393.
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

\seealso{
\code{ \link{disc.mle}, \link{coldisc.mle}
}
}

\examples{
x <- t( rmultinom(1000, 20, c(0.4, 0.5, 0.1) ) )
mvdisc.mle(x, distr = "multinom")
}
