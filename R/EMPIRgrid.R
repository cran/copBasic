"EMPIRgrid" <-
function(para=NULL, deluv=0.05, gridonly=FALSE, verbose=FALSE, ...) {

  if(is.null(para)) {
    warning("parameters (the observed u and v) for empirical copula are NULL")
    return(NULL)
  }
  if(is.list(para)) {
    if(exists("para", para)) {
      para$para <- as.data.frame(para$para)
      if(ncol(para$para) != 2) {
        warning("a data.frame having only two columns is required in the 'para$para' argument")
        return(NULL)
      }
    }
  } else if(ncol(para) != 2) {
     warning("a data.frame having only two columns is required in the 'para' argument")
     return(NULL)
  } else {
    para <- as.data.frame(para)
  }

  us <- seq(0,1, by=deluv);
  vs <- seq(0,1, by=deluv);
  nu <- length(us);
  nv <- length(vs);
  cop <- matrix(nrow=nu, ncol=nv)
  if(verbose) message("Index ")
  for(i in 1:nu) {
     if(verbose) message(nu - i,"-", appendLF=FALSE)
     cop[i,] <- EMPIRcop(rep(us[i], nv), vs, para=para, ...)
  }
  if(verbose) message("done")

  rownames(cop) <- as.character(us)
  colnames(cop) <- as.character(vs)
  if(gridonly) return(cop)
  zzz <- list(u=us, v=us, empcop=cop, deluv=deluv)
  zzz$ctype  <- "unknown"
  dots <- list(...)
  if(exists("ctype", dots)) zzz$ctype <- dots$ctype
  class(zzz) <- c("empirical.copula.grid", class(zzz))
  return(zzz)
}


