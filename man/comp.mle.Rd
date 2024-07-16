\name{MLE of distributions for compositional data}
\alias{comp.mle}
\title{
MLE of distributions for compositional data
}

\description{
MLE of distributions for compositional data.
}

\usage{
comp.mle(x, distr = "diri", type = 1, a = NULL, tol = 1e-07)
}

\arguments{
\item{x}{
A matrix containing the compositional data. Zero values are not allowed except for the case of the ZAD which is designed for the case of zero values present.
}
\item{distr}{
The distribution to fit. "diri" stands for the Dirichlet distribution, "zad" is the Zero Adjusted Dirichlet distribution and "afolded" for the \eqn{\alpha}-folded model (Tsagris and Stewart, 2020).
}
\item{type}{
This is for the Dirichlet distribution ("diri"). Type 1 uses a vectorised version of the Newton-Raphson (Minka, 2012). In high dimensions this is to be preferred. If the data are too concentrated, regardless of the dimensions, this is also to be preferrred. Type 2 uses the regular Newton-Raphson, with matrix multiplications. In small dimensions this can be considerably faster.
}
\item{a}{
The value of \eqn{\alpha}. If this is NULL, the function will estimate it internally.
}
\item{tol}{
The tolerance level idicating no further increase in the log-likelihood.
}
}

\details{
Maximum likelihood estimation of the parameters of a Dirichlet distribution is performed via Newton-Raphson.
Initial values suggested by Minka (2012) are used.
}

\value{
A list including:
\item{loglik}{
The value of the log-likelihood.
}
\item{param}{
The estimated parameters.
}
\item{phi}{
The precision parameter. If covariates are linked with it (function "diri.reg2"), this will
be a vector.
}
\item{mu}{
The mean vector of the distribution.
}
\item{runtime}{
The time required by the MLE.
}
\item{best}{
The estimated optimal \eqn{\alpha} of the folded model.
}
\item{p}{
The estimated probability inside the simplex of the folded model.
}
\item{mu}{
The estimated mean vector of the folded model.
}
\item{su}{
The estimated covariance matrix of the folded model.
}
}

\references{
Minka Thomas (2012). Estimating a Dirichlet distribution. Technical report.

Ng Kai Wang, Guo-Liang Tian, and Man-Lai Tang (2011). Dirichlet and related distributions: Theory, methods and applications. 
John Wiley & Sons.

Tsagris M. and Stewart C. (2018). A Dirichlet regression model for compositional data with zeros.
Lobachevskii Journal of Mathematics, 39(3): 398--412.
Preprint available from https://arxiv.org/pdf/1410.5011.pdf

Tsagris M. and Stewart C. (2022). A Review of Flexible Transformations for Modeling Compositional Data. 
In Advances and Innovations in Statistics and Data Science, pp. 225--234.
https://link.springer.com/chapter/10.1007/978-3-031-08329-7_10

Tsagris M. and Stewart C. (2020). A folded model for compositional data analysis. 
Australian and New Zealand Journal of Statistics, 62(2): 249--277. https://arxiv.org/pdf/1802.07330.pdf
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

\seealso{
\code{\link{prop.mle}
}
}

\examples{
x <- matrix( rgamma(100 * 4, c(5, 6, 7, 8), 1), ncol = 4)
x <- x / rowSums(x)
res <- comp.mle(x)
}
