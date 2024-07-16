colcirc.mle <- function(x, distr = "vm", tol = 1e-07, maxiters = 100, parallel = FALSE) {
  if ( distr == "vm" ) {
    res <- Rfast::colvm.mle(x, tol = tol)
  } else if ( distr == "spml" ) {
    res <- Rfast2::colspml.mle(x, tol = tol, maxiters = maxiters, parallel = parallel)
  }
  res
}
