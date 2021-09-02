"rCOP" <- function(n, cop=NULL, para=NULL, na.rm=TRUE, seed=NULL, ...) {
   if(! is.null(seed)) set.seed(seed)
   u <- runif(n)
   data.frame(U=u, V=simCOPmicro(u, cop=cop, para=para, ...))
}


"simCOP" <-
function(n=100, cop=NULL, para=NULL, na.rm=TRUE, seed=NULL, keept=FALSE,
                graphics=TRUE, ploton=TRUE, points=TRUE, snv=FALSE,
                infsnv.rm=TRUE, trapinfsnv=.Machine$double.eps, ...) {

  if(is.null(cop)) {
     warning("must have copula argument specified, returning NULL")
     return(NULL)
  }

  if(! graphics) {
     ploton <- FALSE
     points <- FALSE
  }

  if(! is.null(seed)) set.seed(seed)

  u <- runif(n); t <- runif(n)
  v <- sapply(1:n, function(i) { derCOPinv(cop=cop, u[i], t[i], para=para, ...) })
  dots <- list(...)
  ditches <- c("delu", "derdir", "trace")
  for(d in ditches) {
    if(d %in% names(dots)) dots <- dots[ - which(names(dots) == d)]
  }

  # Because z is a data.frame, it must be assigned within the ifelse()
  ifelse(keept, z <- data.frame(U=u, V=v, T=t), z <- data.frame(U=u, V=v))
  if(na.rm) {
     z <- z[complete.cases(z), ]
     m <- length(z[,1])
     if(m != n) {
        warning("user requested n=",n," simulations but only m=",m,
                " could be made without NA from derCOPinv (uniroot failure therein)")
        row.names(z) <- NULL # reset the rows to "1:m"
     }
  }
  if(snv) {
     if(infsnv.rm) {
        z <- z[z$U != 0, ]; z <- z[z$U != 0, ]
        z <- z[z$V != 1, ]; z <- z[z$V != 1, ]
        row.names(z) <- NULL
     } else if(trapinfsnv) {
        z$U[z$U == 0] <-   trapinfsnv; z$V[z$V == 0] <-   trapinfsnv
        z$U[z$U == 1] <- 1-trapinfsnv; z$V[z$V == 1] <- 1-trapinfsnv
     }
     z$U <- qnorm(z$U)
     z$V <- qnorm(z$V)
  }
  if(ploton) {
     if(snv) {
        plot(z$U, z$V, type="n",
             xlab="STANDARD NORMAL SCORE FOR U", ylab="STANDARD NORMAL SCORE FOR V")
     } else {
        plot(NA, NA, type="n", xlim=c(0,1), ylim=c(0,1),
             xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
     }
  }
  dots$x <- z$U; dots$y <- z$V
  if(points & ! is.null(dev.list())) do.call("points", dots)

  return(z)
}
