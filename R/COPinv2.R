"COPinv2" <-
function(cop=NULL, v, t, para=NULL, ...) {
    #str(cop)
    func <- function(x,v,LHS,cop,delu=delu,para=para, ...) {
            dc <- COP(cop=cop, v=v, u=x, para=para, ...)
            #print(LHS - dc)
            return(LHS - dc)
    }

    my.rt <- NULL
    try(my.rt <- uniroot(func,interval=c(0,1),
                          v=v, LHS=t, cop=cop, para=para, ...))

    if(is.null(my.rt)) return(NA)
    if(length(my.rt$root) != 0) {
      u <- my.rt$root
      return(u)
    }
    else {
      return(NA)
    }
}
