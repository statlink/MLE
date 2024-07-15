circ.mle <- function(x, distr = "vm", tol = 1e-07, maxiters = 100) {
  if ( distr == "vm" ) {
     res <- Rfast::vm.mle(x, tol = tol)
  } else if ( distr == "spml" ) {
     res <- Rfast::spml.mle(x, tol = tol, maxiters = maxiters)
  } else if ( distr == "wrapcauchy" ) {
     res <- Rfast::wrapcauchy.mle(x, tol = tol)
  }
  res
}
