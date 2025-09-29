"NORMcop" <-
function(u,v, para=NULL, rho=NULL, tau=NULL,
              fit=c('rho', 'tau'), ...) {

  if(is.null(para)) {
    fit <- match.arg(fit)
    zz  <- list(para=c(NA, NA), rho=NA, tau=NA)
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
    zz$para <- p
    names(zz$para) <- c("theta")
    return(zz)
  }

  p  <- para[1]

  if(p < -1 | p > 1) {
    warning("Parameter must be -1 <= Theta <= 1")
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

  # Mimic of copula::normalCopula.R:::pmvnormAlgo
  mimic_pmvnormAlgo <- function(dim, x, checkCorr=FALSE, ...) {
    if(dim <= 3 && ! anyNA(x) && (! any(xI <- x == Inf) || all(xI))) {
      mvtnorm::TVPACK(...)
    } else if(dim <= 5) {
      mvtnorm::Miwa(checkCorr=checkCorr, ...)
    } else {
      mvtnorm::GenzBretz(...)
    }
  }

  upru  <- stats::qnorm(u)
  uprv  <- stats::qnorm(v)
  lwr   <- c(-Inf, -Inf)
  sigma <- matrix(c(1, p, p, 1), nrow=2)

  cop <- sapply(seq_len(length(u)), function(i) { # Mimic of copula::ptCopula
                         x <- c(upru[i], uprv[i]); algo <- mimic_pmvnormAlgo(2, x)
                  mvtnorm::pmvnorm(lower=lwr, upper=x, sigma=sigma, algorithm=algo) })

  # See the comments in Tcop.R for some details about the algorithm therein that do not appear
  # as critical for the NORMcop.R but the mimic_pmvnormAlgo() is used anyway herein to parallel
  # the general nature of the Tcop.R.

  return(cop)
}
