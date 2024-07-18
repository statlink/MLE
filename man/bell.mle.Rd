\name{MLE of Bell type (univariate continuous) distributions}
\alias{bell.mle}
\title{
MLE of Bell type (univariate continuous) distributions
}

\description{
MLE of Bell type (univariate continuous) distributions.
}

\usage{
bell.mle(x, a, b, k, lambda, distr = "BB12", method = "B")
}

\arguments{
\item{x}{
A vector with continuous valued data.
}
\item{a}{
The strictly positive scale parameter of the baseline distribution.
}
\item{b}{
The strictly positive shape parameter of the baseline distribution.
}
\item{k}{
The strictly positive shape parameter of the baseline distribution.
}
\item{lambda}{
The strictly positive parameter of the Bell distribution.
}
\item{distr}{
The distribution to fit, "BB12" stands for the Bell Burr-12, "BBX" for the Bell Burr-10, 
"BE" for the Bell exponential, "BEW" for the Bell exponentiated Weibull, 
"BEE" for the Bell exponentiated exponential, "BF" for the Bell Fisk distribution, 
"BL" for the Bell Lomax, "BW" for the Bell Weibull distribution, 
"CBB12" for the  complementary Bell Burr-12, "CBBX" for the complementary Bell Burr-X distribution, 
"CBE" for the complementary Bell exponential distribution, "CBEW" for the complementary Bell exponentiated Weibul distribution, 
"CBEE" for the complementary Bell extended exponentia distribution, "CBF" for the complementary Bell Fisk distribution, 
"CBL" for the complementary Bell Lomax distribution, and "CBW" for the complementary Bell Weibull distribution.
}
\item{method}{
The procedure for optimising the log-likelihood function after setting the initial values of the parameters and data vector for 
which the Bell-based distributions are fitted. It could be "Nelder-Mead," "BFGS," "CG," "L-BFGS-B," or "SANN." 
"BFGS" is set as the default.
}
}

\details{
These functions facilitate the fitting of Bell-based extended distributions, including the Bell Burr-12(a, b, k, lambda), 
Bell Burr-10(a, lambda), Bell exponential(a, lambda), Bell exponentiated Weibull(a, b, k, lambda), Bell extended exponential(a, b, lambda), 
Bell Fisk(a, b, lambda),  Bell Lomax(a, b, lambda), Bell Weibull(a, b, lambda), complementary  Bell Burr-12(a, b, k, lambda), 
complementary Bell Burr-10(a, lambda), complementary Bell exponential(a, lambda), complementary Bell exponentiated Weibull(a, b, k, lambda), 
complementary Bell extended exponential(a, b, lambda), complementary Bell Fisk(a, b, lambda), complementary Bell Lomax(a, b, lambda), 
and complementary Bell Weibull(a, b, lambda).
}

\value{ 
A list including:
\item{param}{
The parameters of the distribution.
}
\item{loglik}{
The log-likelihood value.
}
}

\references{
Fayomi A., Tahir M. H., Algarni A., Imran M. and Jamal F. (2022). A new useful exponential model with applications to 
quality control and actuarial data. Computational Intelligence and Neuroscience, 2022.

Alanzi, A. R., Imran M., Tahir M. H., Chesneau C., Jamal F. Shakoor S. and Sami, W. (2023). 
Simulation analysis, properties and applications on a new Burr XII model based on the Bell-X functionalities. 
AIMS Mathematics, 8(3): 6970--7004. 

Algarni A. (2022). Group Acceptance Sampling Plan Based on New Compounded Three-
Parameter Weibull Model. Axioms, 11(9): 438.

Kleiber, C. and Kotz, S. (2003). Statistical size distributions in economics and actuarial sciences.
John Wiley & Sons.

Zimmer W. J., Keats J. B. and Wang F. K. (1998). The Burr XII distribution in reliability analysis.
Journal of Quality Technology, 30(4): 386--394.

Nadarajah S., Cordeiro G. M. and Ortega E. M. (2013). The exponentiated Weibull distribution: a survey. 
Statistical Papers, 54: 839--877.

Nadarajah S. (2011). The exponentiated exponential distribution: a survey. Advances in
Statistical Analysis, 95: 219--251.
}

\author{
Muhammad Imran.

R implementation and documentation: Muhammad Imran \email{imranshakoor84@yahoo.com}.
}

\seealso{
\code{ \link{disc.mle}
}
}

\examples{
library(DataSetsUni)
# x is a data vector
x <- DataSetsUni::data_acutebcancer
# Fitting of the Bell Burr-12 (BB12) distribution
bell.mle(x, a = 2.1, b = 1.3, k = 0.02, lambda = 1.2, distr = "BB12", method = "B")
x <- DataSetsUni::data_acutebcancer
# Fitting of the Bell exponential (BE) distribution
bell.mle(x,a = 2.1, lambda = 0.5 ,distr = "BE", method = "B")
#  Fitting of the complementary Bell extended exponential (CBEE) distribution
x <- DataSetsUni::data_guineapigs
bell.mle(x,a = 1.1, b = 2, lambda = 1.4 ,distr = "CBEE", method = "B")
}
