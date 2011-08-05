"derCOPinv" <-
function(cop=NULL, u, t,
         delu=.Machine$double.eps^0.50, para=NULL, ...) {
    #str(cop)
    func <- function(x,u,LHS,cop,delu=delu,para=para, ...) {
            dc <- derCOP(cop=cop, u=u, v=x,
                         delu=delu, para=para, ...)
            #print(LHS - dc)
            return(LHS - dc)
            }
    rt <- NULL;
    try(rt <- uniroot(func,interval=c(0,1),
                      u=u, LHS=t, cop=cop,
                      delu=delu, para=para, ...))

    if(is.null(rt)) return(NA);

    if(length(rt$root) != 0) {
      v <- rt$root
      return(v)
    }
    else {
      return(NA)
    }

}
