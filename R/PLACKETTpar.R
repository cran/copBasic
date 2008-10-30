"PLACKETTpar" <-
function(x, y, rho=NULL, byrho=FALSE, ...) {
   if(! is.null(rho)) byrho <- TRUE
   if(byrho) {
     if(! is.null(rho)) {
       the.rho <- rho
     } else {
       the.rho <- cor(x,y, method="spearman")
     }
     func <- function(x,LHS) {
       if(x == 1) x <- 1.00001
       a <- x + 1; b <- x - 1; c <- b^2
       RHS <- (a/b) - (2*x*log(x))/c
       return(LHS - RHS)
     }
     try(rt <- uniroot(func,
                 interval=c(.Machine$double.eps^0.50,
                            .Machine$double.xmax^0.50),
                 LHS=the.rho))
     return(rt$root)
   }
   else {
     medx <- median(x)
     medy <- median(y)
     k <- length(x[x < medx & y < medy])
     m <- k/length(x)
     return(4*m^2/(1-2*m)^2)
   }
}
