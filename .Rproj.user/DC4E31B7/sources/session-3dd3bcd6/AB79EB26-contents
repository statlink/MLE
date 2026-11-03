truncmle <- function(x, distr = "trunccauchy", a, b, tol = 1e-07) {
  if ( distr == "trunccauchy") {
    res <- Rfast2::trunccauchy.mle(x, a, b, tol = tol)
  } else if ( distr == "truncexpmle" ) {
    res <- Rfast2::truncexpmle(x, b, tol = tol)
  }
  res
}
