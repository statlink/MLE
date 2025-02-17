\name{Column-wise MLE of some circular distributions}
\alias{colcirc.mle}
\title{
Column-wise MLE of some circular distributions
}

\description{
Column-wise MLE of some circular distributions.
}

\usage{
colcirc.mle(x, distr = "vm", N = 2, ina, tol = 1e-07, maxiters = 100, parallel = FALSE)
}

\arguments{
\item{x}{
A numerical matrix with the circular data. They must be expressed in radians.
}
\item{distr}{
The type of distribution to fit, "vm" stands for the von Mises, "spml" is the angular Gaussian, "purka" is the Purkayastha, and "wrapcauchy" is the wrapped Cauchy distribution, "circexp" and "circbeta" stand for the circular exponential and the circular beta distributions, respectively. "cardio" is the cardioid distribution and "ggvm" is the generalized von Mises distribution, "cipc" is the circular independent projected Cauchy, "gcpc" is the generalised circular projected Cauchy distribution and "mmvm" is the multi-modal von Mises distribution. "multivm" and "multispml" denote the von Mises and the angular Gaussian but for multiple samples.
}
\item{N}{
The number of modes to consider in the multi-modal von Mises distribution.
}
\item{ina}{
A numerical vector with discrete numbers starting from 1, i.e. 1, 2, 3, 4,... or a factor variable. Each number denotes a sample or group. If you supply a continuous valued vector the function will obviously provide wrong results. This is only for "multivm" and "multispml".
}
\item{tol}{
The tolerance level to stop the iterative process of finding the MLEs.
}
\item{maxiters}{
The maximum number of iterations to implement. This is for the "spml" only.
}
\item{parallel}{
Should the computations take place in parallel? This is for the "spml" only.
}
}

\details{
The parameters of the von Mises, the bivariate angular Gaussian and wrapped Cauchy distributions are estimated.
For the wrapped Cauchy, the iterative procedure described by Kent and Tyler (1988) is used.
As for the von Mises distribution, we use a Newton-Raphson to estimate the concentration parameter.
The angular Gaussian is described, in the regression setting in Presnell et al. (1998).
}

\value{
A matrix with two, columns. The first one contains the parameters of the distribution
and the second columns contains the log-likelihood values.
}

\references{
Mardia K. V. and Jupp P. E. (2000). Directional statistics. Chicester: John Wiley & Sons.

Sra S. (2012). A short note on parameter approximation for von Mises-Fisher distributions:
and a fast implementation of \eqn{I_s(x)}. Computational Statistics, 27(1): 177--190.

Presnell Brett, Morrison Scott P. and Littell Ramon C. (1998). Projected multivariate linear models for directional data. Journal of the American Statistical Association, 93(443): 1068--1077.

Kent J. and Tyler D. (1988). Maximum likelihood estimation for the wrapped Cauchy distribution.
Journal of Applied Statistics, 15(2): 247--254.

Dietrich T. and Richter W. D. (2017). Classes of geometrically generalized von Mises distributions.
Sankhya B, 79(1): 21--59.

https://en.wikipedia.org/wiki/Wrapped_exponential_distribution

Jammalamadaka S. R. and Kozubowski T. J. (2003). A new family of circular models:
The wrapped Laplace distributions.
Advances and Applications in Statistics, 3(1), 77--103.

Tsagris M. and Alzeley O. (2025). Circular and spherical projected Cauchy distributions:
A Novel Framework for Circular and Directional Data Modelling.
Australian & New Zealand Journal of Statistics (accepted for publication).
https://arxiv.org/pdf/2302.02468.pdf

Barnett M. J. and Kingston R. L. (2024). A note on the Hendrickson-Lattman phase probability
distribution and its equivalence to the generalized von Mises distribution.
Journal of Applied Crystallography, 57(2).

Purkayastha S. (1991). A Rotationally Symmetric Directional Distribution: Obtained through Max-
imum Likelihood Characterization. The Indian Journal of Statistics, Series A, 53(1): 70--83.

Cabrera J. and Watson G. S. (1990). On a spherical median related distribution. Communications
in Statistics-Theory and Methods, 19(6): 1973--1986
}

\author{
Michail Tsagris, Sofia Piperaki and Rafail Vargiakakis.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}, Sofia Piperaki  \email{sofiapip23@gmail.com} and Rafail Vargiakakis \email{rafailvargiakakis@gmail.com}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{circ.mle},
}
}

\examples{
x <- matrix( rnorm(100 * 10, 3, 1), ncol = 10)
x <- x / sqrt( rowSums(x^2) )
res <- colcirc.mle(x, distr = "spml")
}

