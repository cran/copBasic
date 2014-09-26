"tauCOP" <-
function(cop=NULL,  para=NULL,
         cop2=NULL, para2=NULL, brute=FALSE, delta=0.002, ...) {

   if(is.null(cop2)) {
     cop2  <- cop  # This is the expected operation
     para2 <- para # as Kendall's Tau gets returned
   }

   us <- vs <- seq(.Machine$double.eps, 1-.Machine$double.eps, delta)
   if(brute) {
     sum <- 0
     for(u in us) {
       tmp <- sapply(vs, function(v) {
                     return( derCOP(u,v, cop=cop,  para=para,  ...) *
                            derCOP2(u,v, cop=cop2, para=para2, ...))
                         } )
       sum <- sum + sum(tmp)
     }
     Q <- 4*(0.5 - sum*delta^2) - 1 # SEE P 164 of NELSON 2006!
     if(Q >  1) Q <-  1 # assume rounding errors just breaking through
     if(Q < -1) Q <- -1 # again for rounding errors
     return(Q)
   }

   myint <- NULL
   try(myint <- integrate(function(u) {
               sapply(u,function(u) {
                 integrate(function(v) {
                  derCOP( u, v, cop=cop,  para=para,  ...) *
                 derCOP2( u, v, cop=cop2, para=para2, ...)},
                         0, 1)$value
             })}, 0, 1))
    if(is.null(myint)) {
        warning("error on integration encountered, swapping copulas, Nelson corollary 5.1.2")
        try(myint <- integrate(function(u) {
               sapply(u,function(u) {
                 integrate(function(v) {
                  derCOP( u, v, cop=cop2,  para=para2, ...)*
                 derCOP2( u, v, cop=cop,   para=para,  ...)},
                         0, 1)$value
             })}, 0, 1))
        if(is.null(myint)) {
           warning("another error on integration encountered, returning NA")
           return(NULL)
        }
    }
    Q <- 4*(0.5 - myint$value) - 1
    if(Q >  1) Q <-  1 # assume rounding errors just breaking through
    if(Q < -1) Q <- -1 # again for rounding errors
    return(Q)
}

