prop.mle <- function(x, distr = "beta", tol = 1e-07) {
  if ( distr == "beta" ) {
    res <- Rfast::beta.mle(x, tol = tol)
  } else if ( distr == "ibeta" ) {
    res <- Rfast::ibeta.mle(x, tol = tol)
  } else if ( distr == "logitnorm" ) {
    res <- Rfast::logitnorm.mle(x)
  } else if ( distr == "hsecant01" ) {
    res <- Rfast::hsecant01.mle(x, tol = tol)
  }
  res 
}