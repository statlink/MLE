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
  }
  res
}


