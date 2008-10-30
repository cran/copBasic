"taildepCOP" <-
function(cop=NULL, para=NULL,
         plotem=FALSE, verbose=FALSE,  tol=1e-6, ...) {
  tol <- 1e-6
  lamu.tmp <- lamu <- 0
  t <- 0.5
  u <- 0; LAMU1 <- vector(mode="numeric"); LAMU2 <- LAMU1
  if(verbose) cat(c("Upper Tail Dependency"))
  while(t < 1 - .Machine$double.eps) {
    lamu.tmp <- 2 - (1-cop(t,t, para=para, ...))/(1-t)
    if(is.nan(lamu.tmp)) break
    u <- u + 1
    if(verbose) cat(c("index",u,"  t=",t,"  lamu=",lamu,"\n"))
    LAMU1[u] <- t; LAMU2[u] <- lamu
    if(abs(lamu - lamu.tmp) < tol) break
    t <- t + (1-t)/2
    lamu <- lamu.tmp
  }
  if(verbose) cat(c("\nLower Tail Dependency"))
  laml.tmp <- laml <- 0
  t <- 0.5
  l <- 0; LAML1 <- vector(mode="numeric"); LAML2 <- LAML1
  while(t > .Machine$double.eps) {
    laml.tmp <- cop(t,t, para=para, ...)/t
    if(is.nan(laml.tmp)) break
    l <- l + 1
    if(verbose) cat(c("index",u,"  t=",t,"  lamu=",laml,"\n"))
    LAML1[l] <- t; LAML2[l] <- laml
    if(abs(laml - laml.tmp) < tol) break
    t <- t - t/2
    laml <- laml.tmp
  }
  if(verbose) cat(c("\n  done."))
  if(plotem) {
    xmin <- qnorm(min(LAMU1,LAML1))
    xmax <- qnorm(max(LAMU1,LAML1))
    ymin <- min(LAMU2,LAML2)
    ymax <- max(LAMU2,LAML2)
    if(is.null(dev.list())) {
      plot(qnorm(LAMU1),LAMU2, type="l",
           xlim=c(xmin, xmax), ylim=c(ymin, ymax),
           xlab="STANDARD NORMAL DEVIATES",
           ylab="UPPER AND LOWER TAIL DEPENDENCY PARAMETER")
    }
    lines(qnorm(LAML1), LAML2)
    points(qnorm(LAMU1[u]), LAMU2[u], cex=0.5)
    points(qnorm(LAMU1[u]), LAMU2[u], pch="U")
    points(qnorm(LAML1[l]), LAML2[l], cex=0.5)
    points(qnorm(LAML1[l]), LAML2[l], pch="L")
  }
  return(list(lambdaU = LAMU2[u],
              lambdaL = LAML2[l],
              source = "taildepCOP"))
}

