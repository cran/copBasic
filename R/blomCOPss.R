blomCOPss <-
function(cop=NULL, para=NULL, uu=rep(0.5, 2), vv=rep(0.5, 2), trap.nan=TRUE,
          as.sample=FALSE, ctype=c("weibull", "hazen", "1/n",
                                   "bernstein", "checkerboard"), ...) {
   # Schmid and Schmidt doi:10.1007/s00184-006-0114-3 --- Abbreviated SS here
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
      return(blomCOPss(cop=EMPIRcop, uu=uu, vv=vv, para=para, ctype=ctype, ...))
   } else {
      if(is.null(cop)) {
         warning("must have copula argument specified, returning NULL")
         return(NULL)
      }
      # This condition seems clear in SS, but such a conditions keeps the
      # computations out of two quadrants. Use of the two other quadrants means
      # that this SS extension to blomCOP() can be used in the matrix products of
      # blomatrixCOP(). So, let us keep this condition commented out:
      #if(any(uu > vv)) { # SS pp.4 uu<=vv |given uu > 0 | vv < 1
      #   warning("at least one in vector uu is greater than one in vv")
      #   #return(NULL)
      #}

      n  <- length(uu)
      gd <- cumprod(uu)[n] + cumprod(1-vv)[n]
      hd <- 1 / ( min(uu) + min(1-vv) - gd)

      C    <- cop(uu[1], uu[2], para=para, ...)
      Cbar <- surfuncCOP(vv[1], vv[2], cop=cop, para=para, ...)
      if(trap.nan) { # Asquith handling as explained. We know the copBasic::PSP needs
        if(is.nan(C))    C    <- 0 # this as PSP does not trap its own NaN
        if(is.nan(Cbar)) Cbar <- 0 # interred with a PSP reflected
      }
      return(hd * (C + Cbar - gd))
   }
}
