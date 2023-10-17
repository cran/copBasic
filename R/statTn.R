"statTn" <-
 function(u, v=NULL, cop=NULL, para=NULL, p=2, proot=FALSE, ...) {
   if(is.null(v)) {
     v <- u[,2]
     u <- u[,1]
   }
   proot  <- ifelse(proot, 1/p, 1)
   parcop <- COP(u, v, cop=cop, para=para, ...)
   # The empirical is called 2nd because it can be CPU intensive
   # and if the parametric call is going to error, let us have it
   # do it first and not waste users time.
   empcop <- EMPIRcopdf(para=data.frame(u,v), ...)$empcop
   (sum((parcop - empcop)^p))^proot
}

