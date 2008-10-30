"wolfCOP" <-
function(cop=NULL,  para=NULL, delta=0.002, ...) {
   sum <- 0
   for(u in seq(  .Machine$double.eps,
                1-.Machine$double.eps,delta)) {
     for(v in seq(  .Machine$double.eps,
                  1-.Machine$double.eps,delta)) {
       myC <- cop(u,v, para=para, ...)
       sum  <- sum + abs(myC - u*v)
     }
   }
   wolf <- 12*sum*delta^2
   return(wolf)
}
