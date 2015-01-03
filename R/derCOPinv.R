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
    my.rt <- NULL
    try(my.rt <- uniroot(func,interval=c(0,1), u=u, LHS=t,
                              cop=cop, delu=delu, para=para, ...))
    if(is.null(my.rt)) return(NA) # Now the returned root is "v"
    ifelse(length(my.rt$root) != 0, return(my.rt$root), return(NA))
}
