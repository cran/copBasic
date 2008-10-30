"giniCOP" <-
function(cop=NULL, para=NULL, byQ=FALSE, delta=0.002, ...) {
   if(byQ == TRUE) {
     A <- tauCOP(cop=cop, para=para, cop2=M, delta=delta, ...)
     B <- tauCOP(cop=cop, para=para, cop2=W, delta=delta, ...)
     gini <- A + B
     return(A+B)
   }
   myC1 <- function(x, para=NULL) {
     return(cop(x,x, para=para, ...))
   }
   myC2 <- function(x, para=NULL) {
     return(cop(x,1-x, para=para, ...))
   }
   C1 <- integrate(myC1, 0, 1, para=para)
   C2 <- integrate(myC2, 0, 1, para=para)
   A <- C1$value; B <- C2$value
   gini <- 4*(A+B) - 2
   return(gini)
}
