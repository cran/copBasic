"rmseCOP" <-
function(u, v=NULL, cop=NULL, para=NULL, ...) {
  if(is.null(v)) {
    v <- u[,2]
    u <- u[,1]
  }
  if(is.null(cop)) {
    warning("must have copula argument specified, returning NULL")
    return(NULL)
  }
  theo.cop <- COP(u, v, cop=cop, para=para, ...)
  emp.cop  <- EMPIRcop(u,v, para=data.frame(u,v), ...)
  n <- length(theo.cop); k<- length(emp.cop)
  if(n != k) return(Inf)
  mse <- mean((theo.cop - emp.cop)^2)
  return(sqrt(mse))
}
