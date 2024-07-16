\name{MLE of (hyper-)spherical distributions}
\alias{hspher.mle}
\title{
MLE of (hyper-)spherical distributions
}

\description{
MLE of (hyper-)spherical distributions.
}

\usage{
hspher.mle(x, distr = "vmf", ina, tol = 1e-07, ell = FALSE)
}

\arguments{
\item{x}{
A matrix with directional data, i.e. unit vectors.
}
\item{distr}{
The distribution to fit. "vmf" stands for the von Mises-Fisher distribution, "multivmf" is for the vMF with multiple groups, "acg" is the angular central Gaussian 
and "iag" stands for the independent angular Gaussian distribution.
}
\item{ina}{
A numerical vector with discrete numbers starting from 1, i.e. 1, 2, 3, 4,... or a factor variable. Each number denotes a sample or group. 
If you supply a continuous valued vector the function will obviously provide wrong results.
}
\item{ell}{
This is for the multivmf.mle only. Do you want the log-likelihood returned? The default value is TRUE. 
}
\item{tol}{
The tolerance value at which to terminate the iterations.  
}
}

\details{
For the von Mises-Fisher, the normalised mean is the mean direction. For the concentration parameter, a Newton-Raphson is implemented. 
For the angular central Gaussian distribution there is a constraint on the estimated covariance matrix; its trace is equal to the number of variables. 
An iterative algorithm takes place and convergence is guaranteed. Newton-Raphson for the projected normal distribution, on the sphere, is implemented as well. Finally, 
the von Mises-Fisher distribution for groups of data is also implemented. 
}

\value{
For the von Mises-Fisher a list including:
\item{loglik}{
The maximum log-likelihood value.
}
\item{mu}{
The mean direction.
}
\item{kappa}{
The concentration parameter.
}
For the multi von Mises-Fisher a list including:
\item{loglik}{
A vector with the maximum log-likelihood values if ell is set to TRUE. Otherwise NULL is returned.
}
\item{mi}{
A matrix with the group mean directions.
}
\item{ki}{
A vector with the group concentration parameters.
}
For the angular central Gaussian a list including:
\item{iter}{
The number if iterations required by the algorithm to converge to the solution.
}
\item{cova}{
The estimated covariance matrix.
}
For the spherical projected normal a list including:
\item{iters}{
The number of iteration required by the Newton-Raphson. 
}
\item{mesi}{
A matrix with two rows. The first row is the mean direction and the second is the mean vector. 
The first comes from the second by normalising to have unit length.
}
\item{param}{
A vector with the elements, the norm of mean vector, the log-likelihood and the log-likelihood of the spherical uniform distribution. 
The third value helps in case you want to do a log-likleihood ratio test for uniformity.
}
}

\references{
Mardia, K. V. and Jupp, P. E. (2000). Directional statistics. Chicester: John Wiley & Sons.

Sra, S. (2012). A short note on parameter approximation for von Mises-Fisher distributions: and a fast implementation of Is(x). 
Computational Statistics, 27(1): 177--190.

Tyler D. E. (1987). Statistical analysis for the angular central Gaussian distribution on the sphere.
Biometrika 74(3): 579-589.

Paine P.J., Preston S.P., Tsagris M and Wood A.T.A. (2018). An Elliptically Symmetric Angular Gaussian Distribution. Statistics and Computing, 28, 689--697. 
}

\author{
Michail Tsagris and Sofia Piperaki.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr} and Sofia Piperaki  \email{sofiapip23@gmail.com}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{circ.mle}
}
}

\examples{
m <- c(0, 0, 0, 0)
s <- cov(iris[, 1:4])
x <- matrix( rnorm(100 * 3), ncol = 3 )
x <- x / sqrt( rowSums(x^2) )
hspher.mle(x, distr = "iag")
}

