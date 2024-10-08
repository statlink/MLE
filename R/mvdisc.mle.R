mvdisc.mle <- function(x, distr = "multinom", tol = 1e-07) {
  if ( distr == "multinom" ) {
    res <- Rfast::multinom.mle(x)
  } else if ( distr == "dirimultinom" ) {
    res <- Rfast::dirimultinom.mle(x , tol = tol)
  } else if ( distr == "bp" ) {
    res <- bivpois::bp.mle(x1 = x, x2 = NULL)
  } else if ( distr == "bp2" ) {
    res <- bivpois::bp.mle2(x1 = x, x2 = NULL)
  }
  res
}
