"blomCOP" <-
function(cop=NULL, para=NULL, as.sample=FALSE,
                  ctype=c("joe", "weibull", "hazen", "1/n",
                                 "bernstein", "checkerboard"), ...) {
   if(as.sample) {
      ctype <- match.arg(ctype)
      if(is.null(para)) {
         warning("Sample Blomqvist's Beta desired but para is NULL, returning NULL")
         return(NULL)
      }
      if(length(names(para)) != 2) {
        warning("para argument must be data.frame having only two columns, returning NULL")
        return(NULL)
      }
      if(ctype == "joe") {
        n <- nrow(para); A <- (1+n)/2
        return((2/ n)*(sum(as.numeric((rank(para[,1]) - A) *
                                      (rank(para[,2]) - A) >= 0))) - 1)
      } else {
        A <- 1/4 # P(1/2, 1/2) = (1/2) * (1/2)
        return(EMPIRcop(0.5, 0.5, para=para, ctype=ctype, ...)/A - 1)
      }
   } else {
      if(is.null(cop)) {
         warning("must have copula argument specified, returning NULL")
         return(NULL)
      }
      blom <- 4*cop(0.5,0.5, para=para, ...) - 1
      return(blom)
   }
}

