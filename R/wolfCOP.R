"wolfCOP" <-
function(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002,
         nlarge=500, usefastgrid=TRUE, ...) {

    if(as.sample) {
      if(is.null(para)) {
         warning("Sample Schweizer-Wolff Sigma desired but para is NULL, ",
                 "returning NULL")
         return(NULL)
      }
      if(length(names(para)) != 2) {
        warning("para argument must be data.frame having only two columns, ",
                "returning NULL")
        return(NULL)
      }
      n <- nrow(para)
      if(usefastgrid & n > nlarge) {
        ef <- EMPIRgrid_fast(para=para, gridonly=FALSE, ...)
        pf <- ef$empcop
        # gu <- gv <- as.numeric( rownames(ef$empcop) ) # a bit more speed
        gu <- as.numeric( rownames(ef$empcop) ) # to use only a single vector of u = v
        for(i in seq_len(nrow(pf))) pf[i,] <- gu * gu[i] # in lieu of gv * gu[i] for instance
               samSIG <- 12 * sum(abs(ef$empcop - pf)) / (n^2 - 1)
        names(samSIG) <- "Sigma:EMPIRgrid_fast"
        return(samSIG)
      } else {
         if(as.sample == -1) message("Sample Schweizer-Wolff Sigma",
                                     "---CPU intensive!")
         # https://www.cs.cmu.edu/~bapoczos/articles/poczos11nipscopula.pdf
         #                                                (August 11, 2015)
         nn <- n^2; ns <- seq_len(n)
         R <- rank(para[,1]); S <- rank(para[,2])
         samSIG <- sum(sapply(ns, function(i) {
                   sum(sapply(ns, function(j) {
                   abs((sum(as.numeric(R <= i & S <= j))/n) - (i*j/nn))
             } )) } ))
               samSIG <- (12/(nn - 1)) * samSIG
        names(samSIG) <- "Sigma:Poczos et al."
        return(samSIG)
      }
   }

   if(is.null(cop)) {
      warning("must have copula argument specified, returning NULL")
      return(NULL)
   }

   if(brute) {
      us <- vs <- seq(.Machine$double.eps, 1-.Machine$double.eps, delta)
      wolf <- sum(sapply(us, function(u) {
                 sum(sapply(vs, function(v) {
                    abs(cop(u,v, para=para, ...) - u*v) })) }))
      wolf <- 12*wolf*delta^2
      #names(wolf) <- "Sigma:brute"
      return(wolf)
   }

   myint <- NULL
   try(myint <- integrate(function(u) {
            sapply(u,function(u) {
                      integrate(function(v) {
                          abs(COP( u, v, cop=cop, para=para, ...) - u*v)},
                      0, 1)$value
            })}, 0, 1) )
   wolf <- ifelse(is.null(myint), NA, 12*myint$value)
   #names(wolf) <- "Sigma:integrate()"
   return(wolf)
}
