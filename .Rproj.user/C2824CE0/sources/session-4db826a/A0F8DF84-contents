matrix.mle <- function(X, distr = "MN") {
  if ( distr == "MN" ) {
    res <- MN::mn.mle(X)
  } else if ( distr == "mfisher" ) {
    res <- Directional::matrixfisher.mle(X)
  }
  res
}
