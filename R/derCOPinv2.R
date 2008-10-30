"derCOPinv2" <- function(cop=NULL, v, t,
                         delv=.Machine$double.eps^0.50,
                         para=NULL, ...) {
    #str(cop)
    func <- function(x,v,LHS,cop,delv=delv,para=para, ...) {
            dc <- derCOP2(cop=cop, u=x, v=v,
                          delv=delv, para=para, ...)
            return(LHS - dc)
            }
    try(rt <- uniroot(func,interval=c(0,1),
                      v=v, LHS=t, cop=cop,
                      delv=delv, para=para, ...))
    if(length(rt$root) != 0) {
      u <- rt$root
      return(u)
    }
    else {
      return(NA)
    }
}
