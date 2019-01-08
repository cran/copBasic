"composite3COP" <-
function(u,v,para,...) {
  if(is.null(para$kappa)) {
    warning("no kappa parameter given")
    return(NULL)
  }
  if(is.null(para$gamma)) {
    warning("no gamma parameter given")
    return(NULL)
  }
  kappa <- para$kappa; kappa.p <- 1 - kappa
  gamma <- para$gamma; gamma.p <- 1 - gamma
  return(u^kappa * v^gamma * composite2COP(u^kappa.p, v^gamma.p, para, ...))
}
