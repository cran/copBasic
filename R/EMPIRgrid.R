"EMPIRgrid" <-
function(para=NULL, deluv=0.05, ...) {

  if(is.null(para)) {
    warning("parameters (the observed u and v) for empirical copula are NULL")
    return(NULL)
  }

  if(length(names(para)) != 2) {
    warning("a data.frame having only two columns is required")
    return(NULL)
  }
  us <- seq(0,1, by=deluv);
  vs <- seq(0,1, by=deluv);
  nu <- length(us);
  nv <- length(vs);
  cop <- matrix(nrow=nv, ncol=nu)
  for(i in 1:nu) {
     for(j in 1:nv) {
        cop[i,j] <- EMPIRcop(us[i],vs[j], para=para)
     }
  }
  rownames(cop) <- as.character(us)
  colnames(cop) <- as.character(vs)

  zzz <- list(u=us, v=us, empcop=cop, deluv=deluv)
  return(zzz)
}


