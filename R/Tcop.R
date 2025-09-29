"Tcop" <-
function(u,v, para=NULL, rho=NULL, tau=NULL, taildep=NULL,
              fit=c('rho', 'tau'), ...) {

  if(is.null(para)) {
    lamUL <- function(p,nu) 2 * stats::pt(-(sqrt(nu+1)*sqrt(1-p)) / sqrt(1+p), df=nu+1)
    fit   <- match.arg(fit)
    zz    <- list(para=c(NA, NA), rho=NA, tau=NA, lambdaUL=NA, message=NA, lambdaUL_for_nueqone=NA)
    if(is.null(tau) & is.null(rho)) {
      if(fit == "rho") {
        zz$rho <- cor(u,v, method="spearman")
        p <- 2 * sin(pi/6 * zz$rho)
      } else {
        zz$tau <- cor(u,v, method="kendall")
        p <-     sin(pi/2 * zz$tau)
      }
    } else {
      if(fit == "rho") {
        p <- 2 * sin(pi/6 *    rho)
        zz$rho <- rho
      } else {
        p <-     sin(pi/2 *    tau)
        zz$tau <- tau
      }
    }
    lamMAX <- lamUL(p, 1)
    if(lamMAX < taildep) {
      txt <- paste0("computed maximum tail dependence (", lamMAX, ") for Theta=", p, " with nu=1",
                    " is less than given as argument taildep=", taildep,
                    " and therefore uniroot() for the nu will not be possible")
      warning(txt)
      zz$message <- txt
      zz$para <- c(p, NA)
      names(zz$para) <- c("theta", "nu")
      zz$lambdaUL_if_nu_were_eqone <- lamUL(p, 1)
      return(zz)
    }
    rt <- NULL
    try(rt <- uniroot(function(nu, taildep=0, p=NA) {
                        err <- taildep - lamUL(p, nu); return(err) },
                          lower=.Machine$double.eps, upper=1E6, p=p, taildep=taildep), silent=TRUE )
    if(is.null(rt)) {
      txt <-  paste0("computed rho = ", zz$rho, " or computed tau = ", zz$tau, " but unable to",
                     " uniroot() solution for nu for the given tail dependence = ", taildep)
      zz$message <- txt
      return(zz)
    }
    zz$message <- "OK"
    nu <- as.integer(round(rt$root, digits=0))
    zz$para <- c(p, nu)
    names(zz$para) <- c("theta", "nu")
    zz$lambdaUL <- lamUL(p, nu)
    zz$lambdaUL_if_nu_were_eqone <- lamUL(p, 1)
    return(zz)
  }

  p  <- para[1]
  nu <- as.integer(round(para[2], digits=0))

  if(p < -1 | p > 1) {
    warning("Parameter must be -1 <= Theta <= 1")
    return(NULL)
  }
  if(nu < 1) {
    warning("Parameter must be nu >= 1 (can be noninteger)")
    return(NULL)
  }


  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ", length(u), " and length v = ", length(v))
    warning("longer object length is not a multiple of shorter object length, ",
            "no recycling")
    return(NA)
  }

  if(length(u) == 1) {
    u <- rep(u, length(v))
  } else if (length(v) == 1) {
    v <- rep(v, length(u))
  }

  # Mimic of copula::tCopula.R:::pmvtAlgo
  mimic_pmvtAlgo <- function(dim, x, ...) {
    if(dim <= 3 && ! anyNA(x) && (! any(xI <- x == Inf) || all(xI))) {
      mvtnorm::TVPACK(...)
    } else {
      mvtnorm::GenzBretz(...)
    }
  }

  upru  <- stats::qt(u, df=nu)
  uprv  <- stats::qt(v, df=nu)
  lwr   <- c(-Inf, -Inf)
  sigma <- matrix(c(1, p, p, 1), nrow=2)

  cop <- sapply(seq_len(length(u)), function(i) { # Mimic of copula::ptCopula
                         x <- c(upru[i], uprv[i]); algo <- mimic_pmvtAlgo(2, x)
                  mvtnorm::pmvt(lower=lwr, upper=x, sigma=sigma, df=nu, algorithm=algo) })

  # UVo <- simCOP(1000, cop=Tcop, para=c(0.99, 1), seed=10)  # algorithm=TVPACK fixed
  # cop[u == 1] <- v # Note that if TVPACK is always the algorithm that we can seemingly intercept or
  # cop[v == 1] <- u # hack
  # Warning message: In simCOP(1000, cop = Tcop, para = c(0.99, 1)) :
  # user requested n=1000 sims but only m=750 could be made without
  # NA from derCOPinv (uniroot failure therein)

  # UVn <- simCOP(1000, cop=Tcop, para=c(0.99, 1), seed=10) # algorithm=mimic_pmvtAlgo
  # plot(UVo$V-UVn$V) # differences are very small with the
  #   TVPACK & cop[u == 1] <- v & cop[u == 1] <- u
  # that then is evidence that the blowups in the numerics can seemingly be trapped by enforcing
  # fundamental property of grounding the copula by cop[u == 1] <- v & cop[u == 1] <- u

  return(cop)
}
