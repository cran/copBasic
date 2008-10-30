"derCOP" <-
function(cop=NULL, u, v,
         delu=.Machine$double.eps^0.50,
         derdir=c("left", "right", "center"), ...) {

    derdir <- match.arg(derdir)
    #str(cop)
    if(derdir == "left") {
      return((cop(u+delu,v,...) - cop(u,v,...))/delu)
    } else if(derdir == "right") {
      return((cop(u,v,...)      - cop(u-delu,v,...))/delu)
    } else {
      return((cop(u+delu,v,...) - cop(u-delu,v,...))/(2*delu))
    }
}
