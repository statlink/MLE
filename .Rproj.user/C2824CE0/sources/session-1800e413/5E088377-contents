\name{MLE of some censored models}
\alias{cens.mle}
\title{
MLE of some censored models
}

\description{
MLE of some censored models.
}

\usage{
cens.mle(x, distr = "tobit", di, tol = 1e-07)
}

\arguments{
\item{x}{
A vector with positive valued data and zero values. If there are no zero values, a simple normal model is fitted in the end.
}
\item{distr}{
The ditribution to fit. "tobit" stands for the tobit model, "censweibull" for the censored Weibull and "censpois" for the left censored Poisson. For the "censpois" the lowest value in x is taken as the censored point and values below that number are considered to be censored.
}
\item{di}{
A vector of 0s (censored) and 1s (not censored) values.
}
\item{tol}{
The tolerance level up to which the maximisation stops; set to 1e-07 by default.
}
}

\details{
The tobin model is useful for (univariate) positive data with left censoring at zero. There is the assumption of a latent variable. Tthe values of that variable which are positive concide with the observed values. If some values are negative, they are left censored and the observed values are zero. Instead of maximising the log-likelihood via a numerical optimiser we have used a Newton-Raphson algorithm which is faster.
}

\value{
A list including:
\item{iters}{
The number of iterations required for the Newton-Raphson to converge.
}
\item{loglik}{
The value of the maximised log-likelihood.
}
\item{param}{
The vector of the parameters.
}
}

\references{
Tobin James (1958). Estimation of relationships for limited dependent variables. Econometrica. 26(1):24--36.

https://en.wikipedia.org/wiki/Tobit_model

Fritz Scholz (1996). Maximum Likelihood Estimation for Type I Censored Weibull Data Including Covariates. Technical report. ISSTECH-96-022, Boeing Information & Support Services, P.O. Box 24346, MS-7L-22.

}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

\seealso{
\code{ \link{colcens.mle}, \link{positive.mle}, \link{real.mle}
}
}

\examples{
x <- rnorm(300, 3, 5)
x[ x < 0 ] <- 0   ## left censoring. Values below zero become zero
cens.mle(x, distr = "tobit")

x1 <- rpois(10000, 15)
x <- x1
x[x <= 10] <- 10
mean(x) ## simple Poisson
cens.mle(x, distr = "censpois")$lambda

}

