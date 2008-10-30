"COPinv" <-
function(cop=NULL, u, t, para=NULL, ...) {
    if(u == t) return(1)
    #str(cop)
    func <- function(x,u,LHS,cop,delu=delu,para=para, ...) {
            dc <- COP(cop=cop, u=u, v=x, para=para, ...)
            #cat(c(x,u,(LHS - dc),"\n"))
            return(LHS - dc)
    }
    try(rt <- uniroot(func,interval=c(0,1),
                      u=u, LHS=t, cop=cop, para=para, ...))
    if(length(rt$root) != 0) {
      v <- rt$root
      return(v)
    }
    else {
      cat(c(u,t,"\n"))
      return(NA)
    }
}
