hspher.mle <- function(x, distr = "vmf", ina, tol = 1e-07, ell = FALSE) {
   if ( distr == "vmf" ) {
     res <- Rfast::vmf.mle(x, tol = tol)
   } else if ( distr == "multivmf" ) {
     res <- Rfast::multivmf.mle(x , ina, tol = tol, ell = ell)
   } else if ( distr == "acg" ) {
     res <- Rfast::acg.mle(x, tol = tol)
   } else if ( distr == "iag" ) {
     res <- Rfast::iag.mle(x, tol = tol)
   }
   res
}