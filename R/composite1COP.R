"composite1COP" <-
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

  if(is.null(para$para)) para$para <- para$para1
  if(is.null(para$cop )) para$cop  <- para$cop1

  return(u^alpha * v^beta * COP(u^alpha.p, v^beta.p, para=para, ...))
}

"khoudraji1COP" <-
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

  if(is.null(para$para)) para$para <- para$para1
  if(is.null(para$cop )) para$cop  <- para$cop1

  return(u^alpha * v^beta * COP(u^alpha.p, v^beta.p, para=para, ...))
}


"khoudrajiPCOP" <-
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

  if(is.null(para$para)) para$para <- para$para1
  if(is.null(para$cop )) para$cop  <- para$cop1

  return(u^alpha * v^beta * COP(u^alpha.p, v^beta.p, para=para, ...))
}
