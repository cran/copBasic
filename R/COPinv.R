"COPinv" <-
function(cop=NULL, u, t, para=NULL, ...) {
    if(u == t) return(1)
    #str(cop)
    func <- function(x,u,LHS,cop,delu=delu,para=para, ...) {
            dc <- COP(cop=cop, u=u, v=x, para=para, ...)
            #cat(c(x,u,(LHS - dc),"\n"))
            return(LHS - dc)
    }
    my.rt <- NULL
    try(my.rt <- uniroot(func,interval=c(0,1),
                         u=u, LHS=t, cop=cop, para=para, ...))
    
    if(is.null(my.rt)) return(NA);

    if(length(my.rt$root) != 0) {
      v <- my.rt$root
      return(v)
    }
    else {
      cat(c(u,t,"\n"))
      return(NA)
    }
}
