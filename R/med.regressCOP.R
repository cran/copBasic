"med.regressCOP" <-
function(u=seq(0.01,0.99, by=0.01), cop=NULL, para=NULL, level=NA, ...) {
   if(is.null(cop)) {
      warning("must have copula argument specified, returning NULL")
      return(NULL)
   }
   UV <- qua.regressCOP(f=0.5, u=u, cop=cop, para=para, ...)
   if(is.na(level)) return(UV)
   if(length(level) > 1) {
      warning("only the first value of 'level' is used")
      level <- level[1]
   }
   lo <- ifelse(level > 0.5, (1-level)/2, level/2)
   tmp <- UV$V; UV$V <- NULL # copy and then erase
   UV$Vlwr <- qua.regressCOP(f=lo, u=u, cop=cop, para=para, ...)$V
   UV$V    <- tmp # this gets it in the middle
   UV$Vupr <- qua.regressCOP(f=1-lo, u=u, cop=cop, para=para, ...)$V
   return(UV)
}
