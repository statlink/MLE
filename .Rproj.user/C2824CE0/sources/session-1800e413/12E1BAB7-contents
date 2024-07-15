colpositive.mle <- function(x, distr = "gamma", tol = 1e-07, maxiters = 100, parallel = FALSE){
  if ( distr == "gamma" ) {
    res <- Rfast::colgammamle(x, tol = 1e-07)
  } else if ( distr == "weibull" ) {
    res <- Rfast::colweibull.mle(x, tol = tol, maxiters = maxiters, parallel = parallel)
  } else if ( distr == "pareto" ) {
    res <- Rfast::colpareto.mle(x)
  } else if ( distr == "exp" ) {
    res <- Rfast::colexpmle(x)
  } else if ( distr == "exp2" ) {
    res <- Rfast::colexp2.mle(x)
  } else if ( distr == "maxboltz" ) {
    res <- Rfast::colmaxboltz.mle(x)
  } else if ( distr == "lindley" ) {
    res <- Rfast::collindley.mle(x)
  } else if ( distr == "rayleigh" ) {
    res <- Rfast::colrayleigh.mle(x)
  } else if ( distr == "lognorm" ) {
    res <- Rfast2::collognorm.mle(x)
  } else if ( distr == "halfnorm" ) {
    res <- Rfast2::colhalfnorm.mle(x)
  } else if ( distr == "normlog" ) {
    res <- Rfast::colnormlog.mle(x)
  } else if ( distr == "invgauss" ) {
    res <- Rfast::colinvgauss.mle(x)
  } else if ( distr == "powerlaw" ) {
    res <- Rfast2::colpowerlaw.mle(x)
  }
  res
}
