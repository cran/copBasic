"derCOP2" <-
function(cop=NULL, u, v,
         delv=.Machine$double.eps^0.50,
         derdir=c("left", "right", "center"), ...) {

    derdir <- match.arg(derdir)
    if(v - delv < 0) derdir <- "left"
    if(v + delv > 1) derdir <- "right"
    #str(cop)
    if(derdir == "left") {
      return((cop(u,v+delv,...) - cop(u,v,...))/delv)
    } else if(derdir == "right") {
      return((cop(u,v,...)      - cop(u,v-delv,...))/delv)
    } else {
      return((cop(u,v+delv,...) - cop(u,v-delv,...))/(2*delv))
    }
}

