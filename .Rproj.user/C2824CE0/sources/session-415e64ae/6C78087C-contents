colprop.mle <- function(x, distr = "beta", tol = 1e-07, maxiters = 100, parallel = FALSE ){
  if ( distr == "beta" ) {
    res <- Rfast2::colbeta.mle(x, tol = tol)
  } else if ( distr == "logitnorm" ) {
    res <- Rfast2::collogitnorm.mle(x)
  } else if ( distr == "unitweibull" ) {
    res <- Rfast2::colunitweibull.mle(x, tol = tol, maxiters = maxiters, parallel = parallel)
  } else if ( distr == "sp" ) {
    res <- Rfast2::colsp.mle(x)
  }
  res
}
