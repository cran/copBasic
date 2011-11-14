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
    my.rt <- NULL;
    try(my.rt <- uniroot(func,interval=c(0,1),
                         u=u, LHS=t, cop=cop,
                         delu=delu, para=para, ...))

    if(is.null(my.rt)) return(NA);

    if(length(my.rt$root) != 0) {
      v <- my.rt$root
      return(v)
    }
    else {
      return(NA)
    }

}
