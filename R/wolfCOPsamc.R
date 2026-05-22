"wolfCOPsamc" <-
function(u=NULL, v=NULL, cop=EMPIRcop, para=NULL, para_has_paras=FALSE,
         tol=0.005, minit=10, maxit=100, subdivisions=200, large_n=5E3,
         includelmoms=FALSE, trace=FALSE, verbose=FALSE, ...) {

  if(is.null(para)) {
    if(is.null(v)) {
      para <- as.data.frame(u)
    } else {
      para <- data.frame(u=u, v=v)
    }
  }

  if(minit <= 2) minit <- 2
  if(maxit < minit) {
    warning("maxit < minit, swapping them")
    a <- maxit; maxit <- minit; minit <- a; rm(a)
  }

  if(para_has_paras) {
    n <- large_n
  } else {
    n <- nrow(para)
  }

  k <- n^2 # This will fail if nrow(para) makes no sense
  suppressWarnings( if(length(k) == 0) n <- large_n )
  # Error in if (k < subdivisions^2) k <- subdivisions^2 :
  # argument is of length zero
  # So, the conditional failure permits us to guess whether para_has_paras should have been TRUE
  # and the user forgot. The user is using Monte Carlo for Wolf of a copula family and not the sample.

  k <- n^2 # try again, k is going to become the total length of the random number generation where
  # we want only one call to randtoolbox::halton() and then consume those values in the Monte Carlo.
  if(k < subdivisions^2) k <- subdivisions^2 # Override by some testing heuristics.
  if(k > 1E5) k <- 1E5                       # Override by some testing heuristics, do not get too big.
  RT <- randtoolbox::halton(k, dim=2, ...)   # Finally, the matrix for the Monte Carlo, and if this
  # size does not "appear" later as large enough, then runif() calls are made on the fly.

  if(n > large_n) ix <- seq_len(n) # If the sample size is "large", we are going to subset the
  # matrix heading into the empirical copula, we will be drawing randomly these indices for each
  # iteration.

  dtol <- abs(floor(log10(tol)))
  if(dtol < 2) dtol <- 2 # decimal precision to report along the verbose=TRUE messaging system

  MCwolves <- NULL; MCwolfcub <- -9 # Initialization and the -9 is a poison pill for the percent
  # change on the first iteration, which we will hide from the user.
  m <- subdivisions # shorthand
  i <- 0 # initialization
  while(1) { # infinite loop!
    i <- i + 1 # iteration count
    a <- (1+(i-1)*m); b <- m+((i-1)*m) # beginning and ending indices of RT for each Monte Carlo
    #print(c(a, b))
    #if(a > nrow(RT)) break
    if(a > nrow(RT) | b > nrow(RT) | (b - a)+1 < m) { # Silently enter into random number generation
      # print(c("resampling", a, b, m))               # if we have run out of precomputed MC values
      rt <- matrix(runif(m*2), ncol=2)                # in the RT. Defaults settings should be good
    } else {                                          # enough that this seldom occurs in practice.
      #print(c(a,b, nrow(RT)))
      rt <- RT[a:b,] # subset the RT to those indices of the current iteration
    }
    if(n > large_n) { # critical that this is not >= so that we can intercept para_has_paras
      rx <- sample(ix, large_n) # subsampling the indices of the given sample before passing it to copula
      mcwolf <- 12 * sum(sapply(rt[,2], function(v) { # Really here, only the cop=EMPIRcop is viable
               sum(abs(cop(rt[,1],  v, para=para[rx,], ...) - rt[,1] * v )) }) ) / (m^2 - 1)
    } else {
      mcwolf <- 12 * sum(sapply(rt[,2], function(v) { # Here, we can sneak in cop=Copula Family
               sum(abs(cop(rt[,1],  v, para=para,      ...) - rt[,1] * v )) }) ) / (m^2 - 1)
    }
    MCwolves <-    c(MCwolves, mcwolf) # A growing vector of the Schweizer-Wolff sigmas
    MCwolf   <- mean(MCwolves)         # Compute the grand mean of the growing sample size of sigmas
    pctchg   <- abs( 100 * (MCwolf - MCwolfcub) / MCwolfcub ) # Absolute percent change in mean
    # between iterations.

    if(i >= 2) { # because of initialization, only start reporting after the 3rd iteration
      txt <- paste0(sprintf("%2.2i", i), "(", sprintf(paste0("%0.", dtol, "f"),
                                               round(pctchg, digits=dtol)), "%)")
      if(verbose) message(paste0(txt, ":"), appendLF=
                ifelse(length(grep("0$", as.character(i))) != 0, TRUE, FALSE))
    } else {
      txt <- paste0(sprintf("%2.2i", i), "(-.---%)") # hide the opening results, we are always doing 2+
      if(verbose) message(paste0(txt, ":"), appendLF=FALSE)
    }

    if(trace) {
      txt <- paste0(sprintf(   "wolf=%0.3f : ", round(mcwolf, digits=3)), txt,
                    sprintf(" : WOLF=%0.3f",    round(MCwolf, digits=3)))
      if(n > large_n) { # critical that this is not >= so that we can intercept para_has_paras
        plot(para[rx,], pch=1, col="grey80",      lwd=0.8, cex=0.7, las=1,
             xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
      } else {
        plot(para,      pch=1, col="grey80",      lwd=0.8, cex=0.7, las=1,
             xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
      }
      points(rt,       pch=21, col="darkorange4", lwd=0.8, cex=0.6, bg="darkorange1")
      mtext(txt)
    }

    if(i > minit) { if(pctchg < tol | i >= maxit) break } # Only consider breaking after second iteration
    MCwolfcub <- MCwolf
  }
  if(verbose) message("done")

  nmom     <- pmin(5, length(   MCwolves))
  lmr      <- lmomco::lmoms(    MCwolves,               nmom=nmom, no.stop=TRUE)

  #if(abs(MCwolf-lmr$lambdas[1]) > .Machine$double.eps^0.5) {
  #  print(abs(MCwolf-lmr$lambdas[1])); stop("this condition must never happen")
  #}
  yy <- c(sort(c(range(MCwolves), lmr$lambdas[1])))
  names(yy) <- c("min", "mean", "max")
  if(includelmoms) {
    yy <- c(yy, (lmr$lambdas[2]*sqrt(pi))^2, lmr$lambdas[2])
    if(       nmom <  3) {
      yy <- c(yy, NA, NA, NA)
    } else if(nmom == 3) {
      yy <- c(yy, lmr$ratios[3], NA, NA)
    } else if(nmom == 4) {
      yy <- c(yy, lmr$ratios[3:4], NA)
    } else if(nmom >= 5) {
      yy <- c(yy, lmr$ratios[3:5])
    }
    names(yy) <- c("min", "mean", "max", "var", "lscale", "tau3", "tau4", "tau5")
  }

  lmr <- lmomco::lmoms(log(MCwolves/(1-MCwolves)), nmom=nmom, no.stop=TRUE)
  zz <- c(sort(c(range(log(MCwolves/(1-MCwolves))), lmr$lambdas[1])))
  # print(zz)
  names(zz) <- c("min", "mean", "max")
  if(includelmoms) {
    zz <- c(zz, (lmr$lambdas[2]*sqrt(pi))^2, lmr$lambdas[2])
    if(       nmom <  3) {
      zz <- c(zz, NA, NA, NA)
    } else if(nmom == 3) {
      zz <- c(zz, lmr$ratios[3], NA, NA)
    } else if(nmom == 4) {
      zz <- c(zz, lmr$ratios[3:4], NA)
    } else if(nmom >= 5) {
      zz <- c(zz, lmr$ratios[3:5])
    }
    names(zz) <- c("logitmin",    "logitmean", "logitmax",  "logitvar",
                   "logitlscale", "logittau3", "logittau4", "logittau5")
  }
  zz <- list(wolves=MCwolves, estimates=yy, logitestimates=zz, its=i, lastpctchg=pctchg)
  return(zz)
}

#
#uv <- as.data.frame(matrix(runif(20001*2), ncol=2))
#wl <- wolfCOPsamc(para=uv, verbose=TRUE, maxit=500, tol=0.00005, large_n=1E4)

#uv <- simCOP(10000, cop=PSP, graphics=FALSE)
#wc <- wolfCOPsamc(uv, tol=0.00005, verbose=TRUE, usetime=TRUE)
#plot(wc$wolves)
#lines(par()$usr[1:2], rep(wolfCOP(cop=PSP), 2))
#x   <- seq(0,1, by=0.001)
#lmr <- lmomco::vec2lmom(c(wc$logitestimates[c(2,5:8)]))
#gld <- lmomco::parst3(lmr)
#plot(x, dlmomco(log(x/(1-x)), gld), type="l")
