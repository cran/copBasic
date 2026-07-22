"wolfCOPtest" <-
function(x, y, asuv=FALSE, aslist=TRUE, na.rm=TRUE, digits=6,
               probs=c(0.90, 0.95, 0.98, 0.99, 0.995),
               zmat=NULL, statf=mean, rndphi=20, usepade=FALSE,
               ties.method=c("average", "first", "last", "random", "max", "min"), ...) {
  ties.method <- match.arg(ties.method)
  # The probs are quantile levels of the sigma to report, and these are useful to check against the
  # simulations but also to produce these as critical values should the user be interested in
  # these as well as the p-value.
  if(is.null(probs)      ) probs <- 0.95 # 95th percentile or rather the 5-percent critical value (upper tail).
  if( length(probs) == 0 ) probs <- 0.95 # 95th percentile or rather the 5-percent critical value (upper tail).
  if( length(probs) == "") probs <- 0.95 # 95th percentile or rather the 5-percent critical value (upper tail).

  if(! is.null(zmat)) {
    if(ncol(zmat) != 4) {
      warning("zmat when given must be given as four columns, returning NULL")
      return(NULL)
    }
    ties.method <- "random"
    asuv <- FALSE
  }

  lo <- .Machine$double.eps; hi <- 1 - lo
  if(length(x) == 1) { # If x is just one value, then it is treated as the Schweizer-Wolff Sigma and
       rwolf <- x[1]; lwolf <- log( rwolf / (1-rwolf) ); n <- y[1] # the sample size is in y[1]
    if(lwolf == -Inf) lwolf <- log(    lo / (1 - lo)  )
    if(lwolf == +Inf) lwolf <- log(    hi / (1 - hi)  )
    if(n < 3) {
      warning("sample size is <3, returning NULL")
      return(NULL)
    }
    nuuniq <- nvuniq <- rwolves <- "wolf_direct"
  } else {
    # The && is needed to avoid this case
    # Error in if (!is.null(ncol(x)) & ncol(x) == 2) { : argument is of length zero
    if(! is.null(ncol(x)) && ncol(x) == 2) { # This permit the x to be a two column table
      y <- x[,2]; x <- x[,1]                # from which the x,y are formed.
    }
    if(length(x) != length(y)) {
      warning("length x != length y, returning NULL")
      return(NULL)
    }
    uv <- data.frame(u=x, v=y)
    if(! is.null(zmat)) {
      if(nrow(uv) != nrow(zmat)) {
        warning("nrow of uv and zmat are not equal, returning NULL")
        return(NULL)
      }
    }
    if(na.rm) {
      wnt <- stats::complete.cases(uv)
      uv <- uv[wnt,]
      if(! is.null(zmat)) {
        zul <- zmat[wnt,1]; zvl <- zmat[wnt,2]; zur <- zmat[wnt,3]; zvr <- zmat[wnt,4]
      }
    }

    nuuniq <- nvuniq <- rwolves <- NA
    n <- nrow(uv) # sample size
    if(is.null(zmat)) {
      zul <- zvl <- zur <- zvr <- rep(NA, n)
    } else {
      zul <- zmat[,1]; zvl <- zmat[,2]; zvl <- zmat[,3]; zvr <- zmat[,4]
    }

    if(n < 3) { # This handling of the sample size dates from an much earlier version of this
      # function that had a lower limit of 9. With the empirical distributions for sample sizes
      # 3-40 supported, we drop the minimum sample size down to 3 but with the logic here,
      # we effectively permit samples sizes to be incoming down to:
      warning("sample size is <3; returning NULL")
      return(NULL)
    }
    nrndsim <- "zero needed"
    if(any(! is.na(zul)) || any(! is.na(zvl))) {
      if(! is.function(statf)) {
        warning("statf is needed, but it is not a function, returning NULL")
        return(NULL)
      }
      nuuniq <- nvuniq <- "wolves_by_zmatrix"
      nrndsim <- rndphi * (length(zul[! is.na(zul)]) + length(zvl[! is.na(zvl)]))
      ix <- seq_len(n)
      rwolves <- vector(mode="numeric", length=nrndsim)
      for(i in seq_len(nrndsim)) {
        ruv        <- uv; wu <- ! is.na(zul); wv <- ! is.na(zvl)
        ruv[wu, 1] <- sapply(ix[wu], function(k) runif(1, min=zul[k], max=zur[k]))
        ruv[wv, 2] <- sapply(ix[wv], function(k) runif(1, min=zvl[k], max=zvr[k]))
        ruv[   ,1] <- lmomco::pp(ruv[,1], sort=FALSE, ties.method=ties.method, ...)
        ruv[   ,2] <- lmomco::pp(ruv[,2], sort=FALSE, ties.method=ties.method, ...)
        rwolves[i] <- wolfCOP(para=ruv, as.sample=TRUE)
      }
      rwolf <- statf(rwolves)
    } else {
      if(! is.function(statf)) {
        warning("statf is needed, but it is not a function, returning NULL")
        return(NULL)
      }
      nuuniq <- length(unique(uv[,1])); nvuniq <- length(unique(uv[,2]))
      if(! asuv & (nuuniq != n | nvuniq != n) & ties.method == "random") {
        nrndsim <- rndphi * (n - pmin(length(nuuniq), length(nvuniq)))
        rwolves <- vector(mode="numeric", length=nrndsim)
        for(i in seq_len(nrndsim)) {
          ruv        <- uv[sample(seq_len(nrow(uv)), nrow(uv)),]
          ruv[,1]    <- lmomco::pp(ruv[,1], sort=FALSE, ties.method=ties.method, ...)
          ruv[,2]    <- lmomco::pp(ruv[,2], sort=FALSE, ties.method=ties.method, ...)
          rwolves[i] <- wolfCOP(para=ruv, as.sample=TRUE)
        }
        rwolf <- statf(rwolves)
      } else {
       if(! asuv) { # if true, then the user has provided the paired observations of probability
          uv[,1] <- lmomco::pp(uv[,1], sort=FALSE, ties.method=ties.method, ...)
          uv[,2] <- lmomco::pp(uv[,2], sort=FALSE, ties.method=ties.method, ...)
        }
        rwolf <- wolfCOP(para=uv, as.sample=TRUE) # Schweizer-Wolff Sigma : wolf in (0,1)
      }
    }
  }
  lwolf <- log(rwolf / (1 - rwolf)) # logit transform of the Sigma
  if(lwolf == -Inf) lwolf <- log(lo / (1 - lo))
  if(lwolf == +Inf) lwolf <- log(hi / (1 - hi))

  dtype <- ifelse(n <= 40, "gno", "pe3") # We can see via inst/make_wolfCOPtest/chck_wolfCOPtestP.R
  # and the L-moment ratio diagram on the logit transform of the sigma, that there is a heuristic
  # change over from GNO to PE3 at about n=40. HOWEVER ------------------------------------------
  # Study of the terminal output of the make_wolfCOPtestP_smlsam.R script and the upper tail,
  # informs us that the PE3 is reaching into the upper tail already better than the GNO by sample
  # size of 6 to 9 at p-values as fine as 0.001, so we move to the pe3 throughout.
  dtype <- "pe3"

  # Apply the regressions using the hardwired coefficients herein
  # m <- 7000 # sample sizes for which we declare that Tau3 and Tau4 have become constant, which is
  # is technically close to reality but with the curvilinear regression being used, we eschew the
  # prediction not being monotonic decreasing with sample size want it to have an apparent asymptotic.
  m <- ifelse(n > 7000, 7000, n) # This keeps the apparent trajectory of a Tau3 and Tau4 plot
  # having a hook in it as sample sizes increases to infinite.

  if(usepade) { # See copBasic/inst/make_wolfCOPtest/genmod_wolfCOPtestP_B.R
    "myPade" <- function(x, a=0, b=0) { # https://en.wikipedia.org/wiki/Pade_approximant
                   j <- seq_len(length(a))-1; k <- seq_len(length(b))
                   R <- vector(mode="numeric", length(x)) # The response R(x)
                   for(i in seq_len(length(x))) { # for each of the values in x
                     nj <-     sum(sapply(j, function(j) a[j+1]*x[i]^j)) # j=0 to m
                     dk <- 1 + sum(sapply(k, function(k) b[k  ]*x[i]^k)) # k=1 to n
                     R[i] <- nj / dk
                   }
                   return(R) }

    myAlst <- list(
         logitmu   = c(3.22505751817977, -0.915990158877043, 0.967465808418283, -3.40707532868352),
         logitlam2 = c(-4.81815999220734, 10.0896872864149),
         logittau3 = c(1.09499958951764, -0.39965628645059, 0.497606810543813),
         logittau4 = c(0.0963673653663518, -0.180543732512747, 0.14871284556827, 0.234137175245599))
    myBlst <- list(
         logitmu   = c(-0.312728450592508, 3.01615693521397),
         logitlam2 = c(-4.47041708077035, -0.0428413982170646),
         logittau3 = c(-0.36925380858122, 4.22752165636147),
         logittau4 = c(-2.67356792376944, 1.61273757646706, 1.84737967199012))

    #if(n < 6) n <- 6
    mu    <-      myPade(log10(n), a=myAlst$logitmu,   b=myBlst$logitmu  )
    l2    <- exp( myPade(log10(n), a=myAlst$logitlam2, b=myBlst$logitlam2) )
    t3    <-      myPade(log10(m), a=myAlst$logittau3, b=myBlst$logittau3)
    t4    <-      myPade(log10(m), a=myAlst$logittau4, b=myBlst$logittau4)
  } else {
    # Nonlinear regression coefficients computed PRESS minimization of residuals for the
    # exponent on log10(sample size) term. The regressions come from simulation of the Sigma
    # distribution (its logit) assuming the Independence copula.
    mucoe <- c(-0.00568001, -1.09924807, 1.070086,   -1.293310558)
    l2coe <- c( 0.13029126, -0.00099355, 0.09344292, -2.07221688 )
    t3coe <- c( 0.07670933,  0.00566279, 0.16300227, -1.847265638)
    t4coe <- c( 0.12341757, -9.825e-05,  0.04381592, -2.574804698)
    mu    <- mucoe[1] + mucoe[2] * log10(n) + mucoe[3] * log10(n)^mucoe[4] # Mean    (Lambda1)
    l2    <- l2coe[1] + l2coe[2] * log10(n) + l2coe[3] * log10(n)^l2coe[4] # Lambda2 (L-scale)
    t3    <- t3coe[1] + t3coe[2] * log10(m) + t3coe[3] * log10(m)^t3coe[4] # Tau3    (L-skew)
    t4    <- t4coe[1] + t4coe[2] * log10(m) + t4coe[3] * log10(m)^t4coe[4] # Tau4    (L-kurtosis)
  }

  if(t4 < (5 * t3^2 - 1)/4) t4 <- (5 * t3^2 - 1)/4 # theoretical limits of Tau4
  lmrs  <- c(mu, l2, t3, t4) # Tidy list of the Lmoments of the logit(Sigma) distribution
  lmro  <- lmomco::vec2lmom(lmrs, checklmom=FALSE) # Lmoment "o"bject
  if( ! lmomco::are.lmom.valid(lmro) ) {
    warning("L-moments are invalid, sample size beyond empirical logit estimator(s)?\n",
            "   Lambdas ", paste(round(lmro$lambdas, digits=6), collapse=", "), "\n",
            "    Ratios ", paste(round(lmro$ratios,  digits=6), collapse=", "), "\n",
            "Pretrapping of T3-T4 has been made, do you see a negative Lambda2?\n",
            "If so, then that means that the variation is predicted negative!")
    return(NULL)
  }
  if(dtype == "gno") {
    para  <-  lmomco::pargno(lmro, useHosking=FALSE)
  } else if(dtype == "pe3") {
    para  <-  lmomco::parpe3(lmro, useHosking=FALSE)
  } else {
    stop("should not be here in logic")
  }

  # Lmoment ratio diagram shows
  # Really close adherence to a generalized normal distribution (3-parameter log-normal)
  # and hence that distribution is chosen here.
    neps  <-               lmomco::par2cdf(lwolf, para, paracheck=FALSE)   # CDF of logit(SIGMA)
    quans <- 1 / (1 + exp(-lmomco::par2qua(probs, para, paracheck=FALSE))) # QUA of inverse logit --> quans are Sigmas

  quans     <- round(quans,     digits=digits) # Estimated upper-tail quantiles of Sigma distribution
  lmrs      <- round(lmrs,      digits=digits) # Lmoments of the logit(Sigma) distribution
  para$para <- round(para$para, digits=digits) # parameters of the fitted distribution
  rwolf     <- round(rwolf,     digits=digits) # Sigma as the test statistic
  lwolf     <- round(lwolf,     digits=digits) # logit(Sigma) as needed for p-value lookup

  # Because of the potential confusion with the logit() transform involved, let us have long names.
  names(lmrs)      <- c("mu_TEXT_sigmas",    "lscale_TEXT_sigmas",  "tau3_TEXT_sigmas", "tau4_TEXT_sigmas" )
  names(para$para) <- paste0("para", seq_len(length(para$para)), "_TEXT_sigmas")
  names(lmrs)      <- gsub("_TEXT_", "logit", names(lmrs     ))
  names(para$para) <- gsub("_TEXT_", "logit", names(para$para))

  quatxt           <- paste0("fit_f", gsub("\\.", "p", as.character(100 * probs)))
  names(quans)     <- quatxt

  # We appear to use less system time on system.time(), when we find the file and load it instead
  # of using the data() call.
  smlsam <- system.file("data/wolfCOPtest_data_smlsam.RData", package="copBasic")
  max_n_in_smlsam <- 40 # max sample size within wolfCOPtest_data_smlsam$n (yes hard wired)
  n <- as.integer(n)
  if(n <= max_n_in_smlsam & file.exists(smlsam)) {
    wolfCOPtest_data_smlsam <- NULL # initialize whether or no so R CMD check --as-cran will pass by visibility
    load(smlsam)
    #data(wolfCOPtest_data_smlsam) # importFrom("utils", "data") # in NAMESPACE required
    sata <- wolfCOPtest_data_smlsam; suppressWarnings( rm(wolfCOPtest_data_smlsam) )
    sata <- sata[sata$n == n,]       # isolate the table to the sample size of interest
    if(nrow(sata) == 0) {
      warning("sample size n=", n, " does not exist in wolfCOPtest_data_smlsam, so set pval_small=NA")
      pval_small <- NA
      names(pval_small) <- paste0("p.value(sample_le", max_n_in_smlsam, ")")
    } else {
      sata <- sata[order(sata$probs),] # should be sorted already but do so again if needing to inspect
      row.names(sata) <- NULL; # print(sata, 16)
      suppressWarnings( nep_small <- stats::approx(sata$wolfemp, y=sata$probs, xout=rwolf)$y )
      pval_small <- round(1 - nep_small, digits=16)
      names(pval_small) <- paste0("p.value(sample_le", max_n_in_smlsam, ")")
    }
  } else {
    pval_small <- NA; names(pval_small) <- paste0("p.value(sample_le", max_n_in_smlsam, ")")
  }

  pval <- round(1 - neps, digits=16); names(pval) <- paste0("p.value(dist_", dtype, ")")
  pval <- c(pval, pval_small)

  zz <- c(n, rwolf, lwolf, pval, para$para, lmrs, quans, nuuniq, nvuniq, rwolves)
  names(zz) <- c("sample_size", "sigma", "logit_sigma",
                 names(pval), names(para$para), names(lmrs), quatxt,
                 "num_uuniq", "num_vuniq", "rand_sigma")
  names(zz) <- gsub("_TEXT_", "logit", names(zz))

  if(aslist) {
    wz <- c(rwolf, lwolf); names(wz) <- c("sigma", "logit_sigma")
    zz <- list(sample_size=n, estimate=rwolf, statistic=wz, p.value=pval,
               distpara_by_lmoms=para$para)
    zz$lmoms_logit_sigma <- lmrs  # L-moments of the logit(SIGMAS) distribution
    zz$sigma_quantiles   <- quans # Put these last because this length of vector is mutable, and it
    # visually makes these better on the right side of aslist=FALSE (vector return), in particular.
    zz$num_uuniq  <- nuuniq
    zz$num_vuniq  <- nvuniq
    zz$rand_sigma <- rwolves
  }
  return(zz)
}
