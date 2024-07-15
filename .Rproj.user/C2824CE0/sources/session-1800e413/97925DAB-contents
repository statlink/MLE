colreal.mle <- function(x, distr = "normal", tol = 1e-07, maxiters = 100, parallel = FALSE) {
   if ( distr == "normal" ) {
    res <- Rfast::colnormal.mle(x)
   } else if ( distr == "laplace" ) {
    res <- Rfast::collaplace.mle(x)
   } else if ( distr == "cauchy" ) {
    res <- Rfast2::colcauchy.mle(x, tol = 1e-07, maxiters = 100, parallel = FALSE)
   }
   res
}
