"rCOP" <-
function(n, cop=NULL, para=NULL, na.rm=TRUE, seed=NULL,
            resamv01=FALSE, showresamv01=FALSE, ...) {
   if(! is.null(seed)) set.seed(seed)
   u <- runif(n)
   v <- simCOPmicro(u, cop=cop, para=para, ...)
   nn <- length(v[is.na(v) | v <= 0 | v >= 1]) # a smaller infinite break than say n
   runn <- 5; tckr <- NULL
   if(nn != 0 & resamv01) { # only do this block if actually needed
     kk <- 0             # supporting infrastructure to prevent infinite looping
     while(any(is.na(v))) {
       if(showresamv01) message("rCOP() sim'd ", length(v[is.na(v)]), " of v == NA, ", "resampling those")
       v[is.na(v)] <- simCOPmicro(runif(length(v[v <= 0])), cop=cop, para=para, ...)
       kk <- kk + 1      # supporting infrastructure to prevent infinite looping
       if(kk > nn) break # supporting infrastructure to prevent infinite looping
     }
     ii <- 0             # supporting infrastructure to prevent infinite looping
     while(any(is.na(v)) | any(v <= 0)) {
       if(showresamv01) message("rCOP() sim'd ", length(v[v <= 0]), " of v <= 0, ", "resampling those")
       v[is.na(v) | v <= 0] <- simCOPmicro(runif(length(v[v <= 0])), cop=cop, para=para, ...)
       ii <- ii + 1      # supporting infrastructure to prevent infinite looping
       if(ii > nn) break # supporting infrastructure to prevent infinite looping
     }
     jj <- 0             # supporting infrastructure to prevent infinite looping
     while(any(is.na(v)) | any(v >= 1)) {
       if(showresamv01) message("rCOP() sim'd ", length(v[v >= 1]), " of v >= 1, ", "resampling those")
       v[is.na(v) | v >= 1] <- simCOPmicro(runif(length(v[v >= 1])), cop=cop, para=para, ...)
       jj <- jj + 1      # supporting infrastructure to prevent infinite looping
       if(jj > nn) break # supporting infrastructure to prevent infinite looping
     }
     hh <- 0             # supporting infrastructure to prevent infinite looping
     while(any(is.na(v))) {
       if(showresamv01) message("rCOP() still sim'd ", length(v[is.na(v)]), " of v == NA, ", "resampling u,t")
       v[is.na(v)] <- simCOPmicro(runif(length(v[is.na(v)])), cop=cop, para=para, ...)
       hh <- hh + 1      # supporting infrastructure to prevent infinite looping
       if(hh > nn) break # supporting infrastructure to prevent infinite looping
     }
     # still not a perfect solution as pathological situations could at this point now have
     # v <= 0 or v >= 1. But we assured the user that 0 and 1 would not be coming back, so now
     # finally fix by being very close to the edges. But there remains a risk of NAs on v.
     v[v <= 0] <-     .Machine$double.eps^0.50
     v[v >= 1] <- 1 - .Machine$double.eps^0.50
   }
   data.frame(U=u, V=v)
}


"simCOP" <-
function(n=100, cop=NULL, para=NULL, na.rm=TRUE, seed=NULL, keept=FALSE,
                graphics=TRUE, ploton=TRUE, points=TRUE, snv=FALSE,
                infsnv.rm=TRUE, trapinfsnv=.Machine$double.eps,
                resamv01=FALSE, showresamv01=FALSE, ...) {

  if(is.null(cop)) {
     warning("must have copula argument specified, returning NULL")
     return(NULL)
  }

  if(! graphics) {
     ploton <- FALSE
     points <- FALSE
  }

  if(! is.null(seed)) set.seed(seed)

  n <- as.integer(n)

  u <- runif(n); t <- runif(n)
  v <- sapply(seq_len(n), function(i) { derCOPinv(cop=cop, u[i], t[i], para=para, ...) })
  nn <- length(v[is.na(v) | v <= 0 | v >= 1]) # a smaller infinite break than say n
  if(nn != 0 & resamv01) { # only do this block if actually needed
    kk <- 0             # supporting infrastructure to prevent infinite looping
    runn <- 5; tckr <- NULL
    while(any(is.na(v))) {
      if(showresamv01) message("simCOP() sim'd ", length(v[is.na(v)]), " of v == NA, ", "resampling those")
      v[is.na(v)] <- sapply(seq_len(length(v[is.na(v)])),
                     function(i) { derCOPinv(cop=cop, u[i], t[i], para=para, ...) })
      kk <- kk + 1      # supporting infrastructure to prevent infinite looping
      if(kk > nn) break # supporting infrastructure to prevent infinite looping
      tckr <- c(tckr, length(v[is.na(v)]))
      if(length(tckr) > runn) {
        tckr <- tckr[(length(tckr)-runn+1):length(tckr)]
        if(length(unique(tckr)) == 1) break
      }
    }
    ii <- 0             # supporting infrastructure to prevent infinite looping
    tckr <- NULL
    while(any(is.na(v)) | any(v <= 0)) {
      if(showresamv01) message("simCOP() sim'd ", length(v[v <= 0]), " of v <= 0, ", "resampling those")
      v[is.na(v) | v <= 0] <- sapply(seq_len(length(v[v <= 0])),
                            function(i) { derCOPinv(cop=cop, u[i], t[i], para=para, ...) })
      ii <- ii + 1      # supporting infrastructure to prevent infinite looping
      if(ii > nn) break # supporting infrastructure to prevent infinite looping
      tckr <- c(tckr, length(v[is.na(v)]))
      if(length(tckr) > runn) {
        tckr <- tckr[(length(tckr)-runn+1):length(tckr)]
        if(length(unique(tckr)) == 1) break
      }
    }
    jj <- 0             # supporting infrastructure to prevent infinite looping
    tckr <- NULL
    while(any(is.na(v)) | any(v >= 1)) {
      if(showresamv01) message("simCOP() sim'd ", length(v[v >= 1]), " of v >= 1, ", "resampling those")
      v[is.na(v) | v >= 1] <- sapply(seq_len(length(v[v >= 1])),
                            function(i) { derCOPinv(cop=cop, u[i], t[i], para=para, ...) })
      jj <- jj + 1      # supporting infrastructure to prevent infinite looping
      if(jj > nn) break # supporting infrastructure to prevent infinite looping
      tckr <- c(tckr, length(v[is.na(v)]))
      if(length(tckr) > runn) {
        tckr <- tckr[(length(tckr)-runn+1):length(tckr)]
        if(length(unique(tckr)) == 1) break
      }
    }
    hh <- 0             # supporting infrastructure to prevent infinite looping
    tckr <- NULL
    while(any(is.na(v))) {
      if(showresamv01) message("simCOP() still sim'd ", length(v[is.na(v)]), " of v == NA, ", "resampling u,t")
      u[is.na(v)] <- runif(length(v[is.na(v)]))
      t[is.na(v)] <- runif(length(v[is.na(v)]))
      v[is.na(v)] <- sapply(seq_len(length(v[is.na(v)])),
                     function(i) { derCOPinv(cop=cop, u[i], t[i], para=para, ...) })
      hh <- hh + 1      # supporting infrastructure to prevent infinite looping
      if(hh > nn) break # supporting infrastructure to prevent infinite looping
      tckr <- c(tckr, length(v[is.na(v)]))
      if(length(tckr) > runn) {
        tckr <- tckr[(length(tckr)-runn+1):length(tckr)]
        if(length(unique(tckr)) == 1) break
      }
    }
    # still not a perfect solution as pathological situations could at this point now have
    # v <= 0 or v >= 1. But we assured the user that 0 and 1 would not be coming back, so now
    # finally fix by being very close to the edges. But there remains a risk of NAs on v.
    v[v <= 0] <-     .Machine$double.eps^0.50
    v[v >= 1] <- 1 - .Machine$double.eps^0.50
  }

  # stripping content from the ... so that we don't later get the stupid warnings of such and such
  # is not a plotting parameter or versions like that.
  dots <- list(...)
  ditches <- c("delu", "derdir", "trace")
  ditches <- c("pinterval") # for the prod2COP() function
  for(d in ditches) {
    if(d %in% names(dots)) dots <- dots[ - which(names(dots) == d)]
  }

  # Because z is a data.frame, it must be assigned within the ifelse()
  ifelse(keept, z <- data.frame(U=u, V=v, T=t), z <- data.frame(U=u, V=v))
  if(na.rm) {
     z <- z[complete.cases(z), ]
     m <- length(z[,1])
     if(m != n) {
        warning("user requested n=", n, " sims but only m=", m,
                " could be made without\nNA from derCOPinv (uniroot failure therein)")
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
             xlab="STANDARD NORMAL VARIATE OF U",
             ylab="STANDARD NORMAL VARIATE OF V")
     } else {
        plot(NA, NA, type="n", xlim=c(0,1), ylim=c(0,1),
             xlab="U, NONEXCEEDANCE PROBABILITY",
             ylab="V, NONEXCEEDANCE PROBABILITY")
     }
  }
  dots$x <- z$U; dots$y <- z$V
  if(points & ! is.null(dev.list())) do.call("points", dots)

  return(z)
}
