"CIRCcop" <-
function(u, v, para=NULL, as.circ=FALSE, ...) {
  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ",length(u), " and length v = ",length(v))
    warning("longer object length is not a multiple of shorter object length, ",
            "no recycling in CIRC()")
    return(NA)
  }
  if(length(u) == 1) {
     u <- rep(u, length(v))
  } else if(length(v) == 1) {
     v <- rep(v, length(u))
  }
  if(! is.null(para)) {
    if(exists("as.circ", para)) as.circ <- para$as.circ
  }
  if(is.na(as.circ)) as.circ <- FALSE # revert to default
  if(as.circ) {
    u <- 1 - acos(2*u - 1) / pi
    v <- 1 - acos(2*v - 1) / pi
  }

  return(sapply(1:length(u), function(i) {
             if(     abs(u[i] - v[i])     > 1/2) { min(u[i],  v[i])        } # M()
             else if(abs(u[i] + v[i] - 1) > 1/2) { max(u[i] + v[i] - 1, 0) } # W()
             else {     (u[i] + v[i] )/2  - 1/4 } }))                        # otherwise
}
