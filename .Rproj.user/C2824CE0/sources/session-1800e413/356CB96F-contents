disc.mle <- function(x, distr = "poisson", N = NULL, type = 1, tol = 1e-07) {
  if ( distr == "poisson" ) {
    res <- Rfast::poisson.mle(x)
  } else if ( distr == "zip" ) {
    res <- Rfast::zip.mle(x, tol = tol)
  } else if ( distr == "ztp" ) {
    res <- Rfast::ztp.mle(x, tol = tol)
  } else if ( distr == "negbin" ) {
    res <- Rfast::negbin.mle(x, type = type, tol = tol)
  } else if ( distr == "binom" ) {
    res <- Rfast::binom.mle(x, N = N, tol = tol)
  } else if ( distr == "borel" ) {
    res <- Rfast::borel.mle(x)
  } else if ( distr == "geom" ) {
    res <- Rfast::geom.mle(x, type = type)
  } else if ( distr == "logseries" ) {
    res <- Rfast::logseries.mle(x, tol = tol)
  } else if ( distr == "betageom" ) {
    res <- Rfast::betageom.mle(x, tol = tol)
  } else if ( distr == "betabinom" ) {
    res <- Rfast::betabinom.mle(x, N = N, tol = tol)
  } else if ( distr == "skellam" ) {
    res <- skellam::skellam.mle(x)
  } else if ( distr == "gp" ) {
    res <- gp::gp.mle(x)
  }
  res
}
