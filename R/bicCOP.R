"bicCOP" <-
function(u,v, cop=NULL, para=NULL, ...) {
  if(is.null(cop)) {
    warning("must have copula argument specified, returning NULL")
    return(NULL)
  }
  theo.cop <-      cop(u,v, para=para,            ...)
  emp.cop  <- EMPIRcop(u,v, para=data.frame(u,v), ...)
  n <- length(theo.cop); m <- length(emp.cop)
  if(n != m) return(Inf)
  mse <- mean((theo.cop - emp.cop)^2)
  n <- length(emp.cop); m <- length(para)
  bic <- m*log(n) + n*log(mse)
  return(bic)
}
