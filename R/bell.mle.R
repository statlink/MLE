bell.mle <- function(x, a, b, k, lambda, distr = "BB12", method = "B") {
  if ( distr == "BB12" ) {
    res <- BGFD::mBellB(x, a = a, b = b, k = k, lambda = lambda, method = method)
  } else if ( distr == "BBX" ) {
    res <- BGFD::mBellBX(x, a = a, lambda = lambda, method = method)
  } else if ( distr == "BE" ) {
    res <- BGFD::mBellE(x, alpha = a,lambda = lambda, method = method)
  } else if ( distr == "BEW" ) {
    res <- BGFD::mBellEW(x, alpha = a, beta = b, theta = k, lambda = lambda, method = method)
  } else if ( distr == "BEE" ) {
    res <- BGFD::mBellEE(x, alpha = a, beta = b, lambda = lambda, method = method)
  } else if ( distr == "BF" ) {
    res <- BGFD::mBellF(x, a = a, b = b, lambda = lambda, method = method)
  } else if ( distr == "BL" ) {
    res <- BGFD::mBellL(x, b=a, q=b, lambda = lambda, method = method)
   } else if ( distr == "BW" ) {
    res <- BGFD::mBellW(x, alpha = a, beta = b, lambda = lambda, method = method)
   } else if ( distr == "CBB12" ) {
    res <- BGFD::mCBellB(x, a = a, b = b, k=k, lambda = lambda, method = method)
  } else if ( distr == "CBBX" ) {
    res <- BGFD::mCBellBX(x, a = a, lambda = lambda, method = method)
  } else if ( distr == "CBE" ) {
    res <- BGFD::mCBellE(x, alpha = a, lambda = lambda, method = method)
  } else if ( distr == "CBEW" ) {
    res <- BGFD::mCBellEW(x, alpha = a, beta = b, theta = k, lambda = lambda, method = method)
   } else if ( distr == "CBEE" ) {
    res <- BGFD::mCBellEE(x, alpha = a, beta = b, lambda = lambda, method = method)
  } else if ( distr == "CBF" ) {
    res <- BGFD::mCBellF(x, a = a, b = b, lambda = lambda, method = method)
  } else if ( distr == "CBL" ) {
    res <- BGFD::mCBellL(x, b=a, q=b, lambda = lambda, method = method)
  } else if ( distr == "CBW" ) {
    res <- BGFD::mCBellW(x, alpha = a, beta = b, lambda = lambda, method = method)
  } 
  list( param = res$Estimates[, 1], loglik = -0.5 * res[[ 2 ]][5] )
}



