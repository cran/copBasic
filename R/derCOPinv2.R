"derCOPinv2" <-
function(cop=NULL, v, t, trace=FALSE,
         delv=.Machine$double.eps^0.50, para=NULL, ...) {

    func <- function(x,v,LHS,cop,delv=delv,para=para, ...) {
            LHS - derCOP2(cop=cop, u=x, v=v, delv=delv, para=para, ...)
    }
    f.lower <- func(0,v,t,cop,delv=delv,para=para, ...)
    f.upper <- func(1,v,t,cop,delv=delv,para=para, ...)
    if(sign(f.lower) == sign(f.upper)) {
      if(trace) message("not opposite signs for f.lower=",f.lower,
                        " and f.upper=",f.upper,
                        " at v=",v, " and t=",t,
                        "\nThis might be because of degenerate derivative on the section at v.")
      return(NA)
    }

    my.rt <- NULL
    try(my.rt <- uniroot(func,interval=c(0,1), v=v, LHS=t,
                              cop=cop, delv=delv, para=para, ...))
    if(is.null(my.rt)) return(NA) # Now the returned root is "u"
    ifelse(length(my.rt$root) != 0, return(my.rt$root), return(NA))
}
