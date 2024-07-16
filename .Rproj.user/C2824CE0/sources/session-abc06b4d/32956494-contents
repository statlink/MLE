cens.mle <- function(x, distr = "tobit", di, tol = 1e-07) {
  if ( distr == "tobit" ) {
    res <- Rfast::tobit.mle(x, tol = tol)
  } else if ( distr == "censweibull" ) {
    res <- Rfast2::censweibull.mle(x, di, tol = tol)
  } else if ( distr == "censpois" ) {
    res <- Rfast2::censpois.mle(x, tol = tol)
  }
  res
}
