mvdisc.mle <- function(x, distr = "multinom", tol = 1e-07) {
  if ( distr == "multinom" ) {
    res <- Rfast::multinom.mle(x)
  } else if ( distr == "dirimultinom" ) {
    res <- Rfast::dirimultinom.mle(x , tol = tol)
  } 
  res
}