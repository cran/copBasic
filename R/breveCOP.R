"breveCOP" <-
function(u,v, para, ...) {
  if(is.null(para$beta)) {
    warning("no beta parameter given")
    return(NULL)
  }
  if(is.null(para$para)) para$para <- para$para1
  if(is.null(para$cop )) para$cop  <- para$cop1

  beta <- para$beta
  if(para$beta < 0) {
    return( v^(-beta) * COP(u,          v^(1+beta), para=para, ...) )
  } else {
    return( u^(+beta) * COP(u^(1-beta), v,          para=para, ...) )
  }
}

