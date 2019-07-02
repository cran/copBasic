glueCOP <- function(u,v, para=NULL, ...) {
  if(length(para$cop1) == 0) {
    warning("require Copula1 cop1 to be declared")
    return(NULL)
  }
  if(length(para$cop2) == 0) {
    warning("require Copula2 cop2 to be declared")
    return(NULL)
  }
  if(length(para$para1) == 0) {
    warning("require Copula1 parameters para1 to be declared")
    return(NULL)
  }
  if(length(para$para2) == 0) {
    warning("require Copula2 parameters para2 to be declared")
    return(NULL)
  }
  if(is.na(para$glue)) {
    warning("glue needed 0 <= glue <= 1")
    return(NULL)
  }

  # Operationally the following tests are needed although, they are
  # again used inside the COP() call. The compositing functions don't
  # use this tests, but they are here because of the why that
  # u, glue, and v can come in combinations of vector, as in rhoCOP().
  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
     warning("length u = ", length(u), " and length v = ", length(v))
     warning("longer object length is not a multiple of shorter object length, no recycling")
     return(NA)
  }
  if(length(u) == 1) {
     u <- rep(u, length(v))
  } else if (length(v) == 1) {
     v <- rep(v, length(u))
  }
  glue <- para$glue
  cop <- sapply(1:length(u), function(i) {
       if(u[i] <= glue) { glue*COP( u[i]/glue, v[i],
                               cop=para$cop1, para=para$para1, ...)
       } else {       (1-glue)*COP((u[i]-glue)/(1-glue), v[i],
                               cop=para$cop2, para=para$para2, ...) +
                               glue*v[i]
       } })
  return(cop)
}
