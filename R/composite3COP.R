"composite3COP" <-
function(u,v,para,...) {
  kappa <- para$kappa; kappa.p <- 1 - kappa
  gamma <- para$gamma; gamma.p <- 1 - gamma
  C <- composite2COP(u^kappa.p, v^gamma.p, para, ...)
  return(u^kappa * v^gamma * C)
}
