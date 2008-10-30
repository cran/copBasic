"COPinv2" <-
function(cop=NULL, v, t, para=NULL, ...) {
    #str(cop)
    func <- function(x,v,LHS,cop,delu=delu,para=para, ...) {
            dc <- COP(cop=cop, v=v, u=x, para=para, ...)
            #print(LHS - dc)
            return(LHS - dc)
    }
    try(rt <- uniroot(func,interval=c(0,1),
                      v=v, LHS=t, cop=cop, para=para, ...))
    if(length(rt$root) != 0) {
      u <- rt$root
      return(u)
    }
    else {
      return(NA)
    }
}
