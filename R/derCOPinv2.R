"derCOPinv2" <-
function(cop=NULL, v, t,
         delv=.Machine$double.eps^0.50, para=NULL, ...) {
    #str(cop)
    func <- function(x,v,LHS,cop,delv=delv,para=para, ...) {
            dc <- derCOP2(cop=cop, u=x, v=v,
                          delv=delv, para=para, ...)
            #print(LHS - dc)
            return(LHS - dc)
            }
    my.rt <- NULL
    try(my.rt <- uniroot(func,interval=c(0,1),
                         v=v, LHS=t, cop=cop,
                         delv=delv, para=para, ...))
    
    if(is.null(my.rt)) return(NA);

    if(length(my.rt$root) != 0) {
      u <- my.rt$root
      return(u)
    }
    else {
      return(NA)
    }
}
