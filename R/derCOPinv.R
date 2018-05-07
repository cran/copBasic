"derCOPinv" <-
function(cop=NULL, u, t, trace=FALSE,
         delu=.Machine$double.eps^0.50, para=NULL, ...) {

    func <- function(x,u,LHS,cop,delu=delu,para=para, ...) {
            LHS - derCOP(cop=cop, u=u, v=x, delu=delu, para=para, ...)
    }
    f.lower <- func(0,u,t,cop,delu=delu,para=para, ...)
    f.upper <- func(1,u,t,cop,delu=delu,para=para, ...)
    if(sign(f.lower) == sign(f.upper)) {
      if(trace) message("not opposite signs for f.lower and f.upper: u=",u, " and t=",t)
      return(NA)
    }

    my.rt <- NULL
    try(my.rt <- uniroot(func,interval=c(0,1), u=u, LHS=t,
                              cop=cop, delu=delu, para=para, ...))
    if(is.null(my.rt)) return(NA) # Now the returned root is "v"
    ifelse(length(my.rt$root) != 0, return(my.rt$root), return(NA))
}
