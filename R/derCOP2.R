"derCOP2" <-
function(cop=NULL, u, v,
         delv=.Machine$double.eps^0.50,
         derdir=c("left", "right", "center"), ...) {

    derdir <- match.arg(derdir)
    #str(cop)
    if(derdir == "left") {
      return((cop(u,v+delv,...) - cop(u,v,...))/delv)
    } else if(derdir == "right") {
      return((cop(u,v,...)      - cop(u,v-delv,...))/delv)
    } else {
      return((cop(u,v+delv,...) - cop(u,v-delv,...))/(2*delv))
    }
}
