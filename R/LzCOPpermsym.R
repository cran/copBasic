"LzCOPpermsym" <-
function(cop=NULL, para=NULL, n=5E4,
         type=c("halton", "sobol", "torus", "runif"),
         as.abs=TRUE, as.vec=FALSE, as.mat=FALSE, plot=FALSE, ...) {

  type <- match.arg(type)
  n <- as.integer( n ) # to silence certain upstream warnings()

  if(type != "runif") {
    if(! "randtoolbox" %in% installed.packages()) {
      warning("randtoolbox package not installed, switch type to 'runif'")
      type <- 'runif'
    }
  }

  if(type == "runif") {
    ruv <- matrix(runif(2*n),    ncol=2)
  } else if(type == "halton") {
    ruv <- randtoolbox::halton(n, dim=2)
  } else if(type == "sobol" ) {
    ruv <- randtoolbox::sobol(n,  dim=2)
  } else if(type == "torus" ) {
    ruv <- randtoolbox::torus(n,  dim=2)
  }

  if(plot) {
    plot(ruv, main=type,
         xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE")
  }

  if(as.vec | as.mat) as.abs <- FALSE

  ifelse(as.abs, absf <- abs, absf <- function(x) x)

  sym <- absf( COP(ruv[,1], ruv[,2], cop=cop, para=para, ...) -
               COP(ruv[,2], ruv[,1], cop=cop, para=para, ...) )
  if(as.mat) {
    return( cbind(ruv, 3 *        sym ) )
  } else {
    if(as.vec) {
      return(          3 *        sym   )
    } else {
      if(as.abs) {
        return(        3 * max(   sym ) )
      } else {
        return(        3 * range( sym ) )
      }
    }
  }
}
