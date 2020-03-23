"EMPIRgridderinv" <-
function(empgrid=NULL, kumaraswamy=FALSE, dergrid=NULL, ...) {

  if(is.null(empgrid)) {
    warning("The gridded empirical copula (say from EMPIRgrid) is NULL")
    return(NULL)
  }
  if(! is.list(empgrid)) {
    warning("The gridded empirical copula is expected as a list from EMPIRgrid")
    return(NULL)
  }

  if(is.null(dergrid)) {
    the.deriv <- EMPIRgridder(empgrid=empgrid,...)
  } else {
    the.deriv <- dergrid
  }

  FF <- empgrid$v
  n <- length(FF)

  # use a mid-point integration method so 1/2s are needed
  # on the tails.
  delF <- diff(FF)
  delF <- c(0.5*delF[1], delF)
  delF[n] <- 0.5*delF[n]

  the.inverse <- matrix(nrow=n, ncol=n)
  Alphas <- Betas <- vector(mode="numeric", length=n)
  Alphas[1] <- NA; Betas[1] <- NA
  for(i in 2:n) {
    x <- the.deriv[i,]

    if(length(x[! is.finite(x)]) > 0) {
      warning("found nonfinite values on row=",i," in grid inversion")
      next
    }

    # invert the CDF by linear approximation
    # we want the QDF with FF (horizontal axis values on same spacing)
    # we know that the x are given in ordered seqeuence to so avoid
    # the warning
    # In regularize.values(x, y, ties, missing(ties)) :
    # collapsing to unique 'x' values
    inv <- approx(x, y=FF, xout=FF, rule=2, ties="ordered")$y

    if(kumaraswamy) {
      beta0 <- sum(inv*delF) # first PWM (mean)
      beta1 <- sum(inv*FF*delF) # second PWM (no name)

      lmr <- lmomco::vec2lmom(c(beta0, 2*beta1 - beta0)) # L-moments
      par.of.kur <- lmomco::parkur(lmr)
      X <- lmomco::quakur(FF, par.of.kur) # Kumuraswamy distribution
      if(is.null(X)) {
        warning("failed Kumaraswamy fit at i=",i,", using approx inv")
        the.inverse[i,] <- inv
        Alphas[i] <- NA
        Betas[i]  <- NA
        next
      }
      the.inverse[i,] <- X
      Alphas[i] <- par.of.kur$para[1]
      Betas[i]  <- par.of.kur$para[2]
    } else {
       the.inverse[i,] <- inv
       Alphas[i] <- NA
       Betas[i]  <- NA
    }
  }
  attributes(the.inverse) <- list(dim=dim(empgrid$empcop),
                                  rownames=empgrid$u,
                                  colnames=empgrid$v,
                                  kumaraswamy=list(Alpha=Alphas,
                                                   Beta=Betas),
                                  message="use the rows!, wrt U")

  return(the.inverse)
}
