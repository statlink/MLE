mv.mle <- function(x, distr = "mvnorm", v = 1, tol = 1e-7) {
  if ( distr == "mvnorm" ) {
     res <- Rfast::mvnorm.mle(x)
  } else if ( distr == "mvlnorm" ) {
     res <- Rfast::mvlnorm.mle(x)
  } else if ( distr == "mvt" ) {
    res <- Rfast::mvt.mle(x, v = v, tol = tol)
  } else if ( distr == "invdir" ) {
    res <- Rfast::invdir.mle(x, tol = tol)
  }
  res
}
