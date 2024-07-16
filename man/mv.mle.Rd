\name{MLE of some continuous multivariate distributions}
\alias{mv.mle}
\title{
MLE of some continuous multivariate distributions
}

\description{
MLE of some continuous multivariate distributions.
}

\usage{
mv.mle(x, distr = "mvnorm", v = 1, tol = 1e-7)
}

\arguments{
\item{x}{
A matrix with numerical data.
}
\item{distr}{
The distribution to fit. "mvnorm" stands for the multivariate normal distribution,
"mvlnorm" for the multivariate log-normal, "mvt" is the multivariate t distribution and
"invdir" stands for the inverse Dirichlet distribution.
}
\item{v}{
The degrees of freedom. Must be a positive number, greater than zero.
}
\item{tol}{
The tolerance value to terminate the EM algorithm.
}
}

\details{
The mean vector, covariance matrix and the value of the log-likelihood of the
multivariate normal or log-normal distribution is calculated.
For the log-normal distribution we also provide the expected value and the covariance matrix.
The location vector, scatter matrix and the value of the log-likelihood for the multivariate t distribution is calculated.
Maximum likelihood estimation of the parameters of the inverted is performed via Newton-Raphson.
}

\value{
A list including:
\item{loglik}{
The log-likelihood multivariate distribution.
}
\item{mu}{
The mean vector.
}
\item{sigma}{
The covariance matrix.
}
\item{m}{
The expected mean vector of the multivariate log-normal distribution.
}
\item{s}{
The expected covariance matrix of the multivariate log-normal distribution.
}
}

\references{
Kotz S., Balakrishnan N. and Johnson N. L. (2004). Continuous multivariate distributions, Volume 1: Models and applications (Vol. 1). John wiley & sons.

Nadarajah S. and Kotz S. (2008). Estimation methods for the multivariate t distribution. Acta Applicandae Mathematicae, 102(1):99-118.

Bdiri T. and Bouguila N. (2012). Positive vectors clustering using inverted Dirichlet finite mixture models. Expert Systems with Applications, 39(2): 1869-1882.

http://isi.cbs.nl/iamamember/CD2/pdf/329.PDF

https://en.wikipedia.org/wiki/Log-normal_distribution#Multivariate_log-normal
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
x <- matrix( rnorm(100 * 5), ncol = 5)
res <- mv.mle(x)
}
