positive.mle <- function(x, distr = "gamma", tol = 1e-07, maxiters = 100) {
  if ( distr == "gamma" ) {
    res <- Rfast::gammamle(x, tol = tol)
  } else if ( distr == "chisq" ) {
    res <- Rfast::chisq.mle(x, tol = tol)
  } else if ( distr == "weibull" ) {
    res <- Rfast::weibull.mle(x, tol = tol, maxiters = maxiters)
  } else if ( distr == "lomax" ) {
    res <- Rfast::lomax.mle(x, tol = tol)
  } else if ( distr == "foldnorm" ) {
    res <- Rfast::foldnorm.mle(x, tol = tol)
  } else if ( distr == "betaprime" ) {
    res <- Rfast::betaprime.mle(x, tol = tol)
  } else if ( distr == "lognorm" ) {
    res <- Rfast::lognorm.mle(x)
  } else if ( distr == "logcauchy" ) {
    res <- Rfast::logcauchy.mle(x, tol = tol)
  } else if ( distr == "loglogistic" ) {
    res <- Rfast::loglogistic.mle(x, tol = tol)
  } else if ( distr == "normlog" ) {
    res <- Rfast::normlog.mle(x)
  } else if ( distr == "halfnorm" ) {
    res <- Rfast::halfnorm.mle(x)
  } else if ( distr == "invgauss" ) {
    res <- Rfast::invgauss.mle(x)
  } else if ( distr == "pareto" ) {
    res <- Rfast::pareto.mle(x)
  } else if ( distr == "exp" ) {
    res <- Rfast::expmle(x)
  } else if ( distr == "exp2" ) {
    res <- Rfast::exp2.mle(x)
  } else if ( distr == "maxboltz" ) {
    res <- Rfast::maxboltz.mle(x)
  } else if ( distr == "rayleigh" ) {
    res <- Rfast::rayleigh.mle(x)
  } else if ( distr == "lindley" ) {
    res <- Rfast::lindley.mle(x)
  } else if ( distr == "zigamma") {
    res <- Rfast2::zigamma.mle(x, tol = tol)
  } else if ( distr == "ziweibull" ) {
    res <- Rfast2::ziweibull.mle(x, tol = tol)
  } else if (distr == "epois" ) {
    res <- geppe::epois.mle(x)
  } else if (distr == "gep" ) {
    res <- geppe::gep.mle(x)
  } else if (distr == "pe" ) {
    res <- geppe::pe.mle(x)
  } else if ( distr == "halfcauchy" ) {
    res <- Rfast2::halfcauchy.mle(x, tol = tol )
  } else if ( distr == "powerlaw" ) {
    res <- Rfast2::powerlaw.mle(x)
  } else if (distr == "wp" ) {
    res <- .wp.mle(x)
  } else if (distr == "be" ) {
    res <- .be.mle(x)
  } else if (distr == "frechet2" ) {
    res <- .fng.mle(x)
  }
  res
}


.wp.mle <- function(x) {

  wp <- function(vec, x, slx, n) {
    alpha <- exp( vec[1] )
    b <- exp( vec[2] )
    lambda <- exp( vec[3] )
    logca <- log(alpha * b * lambda) - lambda - log(1 - exp(- lambda) )
     -n * logca + b * sum( x^alpha ) - (alpha - 1) * slx - lambda * sum( exp(- b * ( x^alpha ) ) )
  }

  n <- length(x)  ;  slx <- sum( log( x ) )
  f <- optim( par = c(1, 1, 1), fn = wp, x = x, slx = slx, n = n, control = list(maxit = 5000) )
  f <- optim( par = f$par, fn = wp, x = x, n = n, slx = slx, control = list(maxit = 5000) )
  param <- exp( f$par )
  names(param) <- c("alpha", "beta", "lambda")
  list(param = param, loglik = -f$value)
}


.be.mle <- function(x) {

  be <- function(vec, x, sx, n) {
    alpha <- exp( vec[1] )
    b <- exp( vec[2] )
    lambda <- exp( vec[3] )
    -n * log( lambda ) + n * lbeta( b, alpha ) + alpha * lambda * sx - (b - 1) * sum( log(1 - exp(- lambda * x ) ) )
  }

  n <- length(x)  ;  sx <- sum(x)
  f <- optim( par = c(1, 1, 1), fn = be, x = x, sx = sx, n = n, control = list(maxit = 5000) )
  f <- optim( par = f$par, fn = be, x = x, n = n, sx = sx, control = list(maxit = 5000) )
  param <- exp( f$par )
  names(param) <- c("alpha", "beta", "lambda")
  list(param = param, loglik = -f$value)
}


.fng.mle <- function(x) {

  ##- ( n * log( gamma ) - n * log( b ) - ( gamma + 1 ) * sum( log( x / b ) )  -
  ##( ( 1 / b ) ^ ( - gamma ) ) * sum( x ^ ( - gamma ) ) )
  fng <- function(vec, x, slx, n) {
    b <- exp( vec[1] )
    gamma <- exp( vec[2] )
    - n * log( gamma ) + n * log( b ) + ( gamma + 1 ) * ( slx - n * vec[1] ) +
      ( ( 1 / b ) ^ ( - gamma ) ) * sum( x ^ ( - gamma ) )
  }

  mod <- optim( par = c(1, 1), fng, x = x, slx = sum( log(x) ), n = length(x) )
  param <- exp(mod$par)
  names(param) <- c("beta", "gamma")
  list(param = param, loglik = -mod$value)
}


