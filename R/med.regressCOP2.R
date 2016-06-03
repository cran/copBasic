"med.regressCOP2" <-
function(v=seq(0.01,0.99, by=0.01), cop=NULL, para=NULL, level=NA, ...) {
   if(is.null(cop)) {
      warning("must have copula argument specified, returning NULL")
      return(NULL)
   }
   UV <- qua.regressCOP2(f=0.5, v=v, cop=cop, para=para, ...)
   if(is.na(level)) return(UV)
   if(length(level) > 1) {
      warning("only the first value of 'level' is used")
      level <- level[1]
   }
   lo <- ifelse(level > 0.5, (1-level)/2, level/2)
   tmp <- UV$U; UV$U <- NULL # copy and then erase
   UV$Ulwr <- qua.regressCOP2(f=  lo, v=v, cop=cop, para=para, ...)$U
   UV$U    <- tmp # this gets it in the middle
   UV$Uupr <- qua.regressCOP2(f=1-lo, v=v, cop=cop, para=para, ...)$U
   return(UV)
}
