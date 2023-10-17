"breveCOP" <-
function(u,v, para, breve=NULL, ...) {

  if(is.null(para$para)) para$para <- para$para1
  if(is.null(para$cop )) para$cop  <- para$cop1

  the.breve <- NULL
  if(! is.null(breve)) {
    the.breve <- breve
  } else if(! is.null(para$breve)) {
    the.breve <- para$breve
  } else if(! is.null(para$beta )) {
    the.breve <- para$beta
  } else {
    if(is.null(the.breve)) {
      warning("no beta parameter given")
      return(NULL)
    }
  }

  if(       the.breve > +1) {
    the.breve <- +1
  } else if(the.breve < -1) {
    the.breve <- -1
  }

  if(the.breve < 0) {
    return( v^(-the.breve) * COP(u,               v^(1+the.breve),
           para=para, ...) )
  } else {
    return( u^(+the.breve) * COP(u^(1-the.breve), v,
           para=para, ...) )
  }
}
