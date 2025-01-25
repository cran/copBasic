"composite2COP" <-
function(u, v, para, ...) {
  if(is.null(para$alpha)) {
    warning("no alpha parameter given")
    return(NULL)
  }
  if(is.null(para$beta)) {
    warning("no beta parameter given")
    return(NULL)
  }
  alpha <- para$alpha; alpha.p <- 1 - alpha
  beta  <- para$beta;   beta.p <- 1 - beta
  return(COP(cop=para$cop1, u^alpha,   v^beta,   para=para$para1, ...) *
         COP(cop=para$cop2, u^alpha.p, v^beta.p, para=para$para2, ...))
}

"khoudraji2COP" <-
function(u, v, para, ...) {
  if(is.null(para$alpha)) {
    warning("no alpha parameter given")
    return(NULL)
  }
  if(is.null(para$beta)) {
    warning("no beta parameter given")
    return(NULL)
  }
  alpha <- para$alpha; alpha.p <- 1 - alpha
  beta  <- para$beta;   beta.p <- 1 - beta
  return(COP(cop=para$cop1, u^alpha,   v^beta,   para=para$para1, ...) *
         COP(cop=para$cop2, u^alpha.p, v^beta.p, para=para$para2, ...))
}
