prop.mle <- function(x, distr = "beta", tol = 1e-07, maxiters = 50) {
  if ( distr == "beta" ) {
    res <- Rfast::beta.mle(x, tol = tol)
  } else if ( distr == "ibeta" ) {
    res <- Rfast::ibeta.mle(x, tol = tol)
  } else if ( distr == "logitnorm" ) {
    res <- Rfast::logitnorm.mle(x)
  } else if ( distr == "hsecant01" ) {
    res <- Rfast::hsecant01.mle(x, tol = tol)
  } else if ( distr == "kumar" ) {
    res <- Rfast2::kumar.mle(x, tol = tol, maxiters = maxiters)
  } else if ( distr == "simplex" ) {
    res <- Rfast2::simplex.mle(x, tol = tol)
  } else if ( distr == "zil" ) {
    res <- Rfast2::zil.mle(x)
  } else if ( distr == "unitweibull" ) {
    res <- Rfast2::unitweibull.mle(x, tol = tol, maxiters = maxiters)
  } else if ( distr == "cbern" ) {
    res <- Rfast2::cbern.mle(x, tol = tol)
  } else if ( distr == "sp" ) {
    res <- Rfast2::sp.mle(x, tol = tol)
  }
  res
}
