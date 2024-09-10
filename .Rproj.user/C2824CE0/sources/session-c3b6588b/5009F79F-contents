comp.mle <- function(x, distr = "diri", type = 1, a = NULL, tol = 1e-07) {
  if ( distr == "diri" ) {
    res <- Rfast::diri.nr2(x, type = type, tol = tol)
  } else if ( distr == "zad" ) {
    res <- Compositional::zad.est(x)
  } else if ( distr == "afolded" ) {
    if ( !is.null(a) ) {
      res <- Compositional::alpha.mle(x, a)
    } else {
      res <- Compositional::a.est(x)
    }
  }
  res
}
