"JOMAcop" <- # Joe (2014, p. 177)
function(u, v, para=NULL, rhotau=NULL, cortype=c("kendall", "spearman", "tau", "rho"), ...) {
  cortype <-   tolower(cortype)
  cortype <- match.arg(cortype)
  if(is.null(para)) {
    if(cortype == "spearman" | cortype == "rho") {
      if(is.null(rhotau)) {
        rho <- cor(u,v, method="spearman")
      } else {
        rho <- rhotau
      }
      ofunc_rho <- function(trial_para, target_rho=NA) {
        rho <- rhoCOP(cop=JOMAcop, para=trial_para)
        return(target_rho - rho)
      }
      para <- uniroot(ofunc_rho, interval=c(-1, +1), target_rho=rho)$root
      names(para) <- "theta"
      names(rho ) <- "Spearman Rho"
      return(list(para=para, rho=rho))
    } else if(cortype == "kendall" | cortype == "tau") {
      if(is.null(rhotau)) {
        tau <- cor(u,v, method="kendall")
      } else {
        tau <- rhotau
      }
      ofunc_tau <- function(trial_para, target_tau=NA) {
        tau <- tauCOP(cop=JOMAcop, para=trial_para)
        return(target_tau - tau)
      }
      para <- uniroot(ofunc_tau, interval=c(-1, +1), target_tau=tau)$root
      names(para) <- "theta"
      names(tau ) <- "Kendall Tau"
      return(list(para=para, tau=tau))
    } else {
      stop("should not be here in logic")
    }
    }

    if(para[1] < -1) {
      warning("Parameter Delta < -1")
      return(NULL)
    } else if(para[1] > +1) {
      warning("Parameter Delta > +1")
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
    } else if(length(v) == 1) {
       v <- rep(v, length(u))
    }

    d <- para[1]; t <- sign(d)  # This is departure from Joe and Ma parameterization as I bend the
    d <- 1 - abs(d); k <- 1 / d # rules to make the copula in (-1, +1) as part of extensive testing.

    # These break points come from simulation testing of the numerical derivative and the behavior
    # of tauCOP() on this copula. The Gamma distribution can be very difficult as shape parameter
    # goes "small" and I have not found a work around other than the two thresholds here and the
    # whole business of the d in (-1, +1) for this parameterization is that the d > 1 + d --> infinity
    # breaks down far to early in the derCOPinv() [including also Joe's direct inversion formula]
    # for suitability to copBasic. I remark that the R code base coming with Joe's book does not
    # appear to go this far in study of the numerical limitations of the Gamma distribution and
    # asymmetry whether the relation is negative associated or positive associated.
    if(d < 0.02 & d > 0.013) {
      Hcdf <- function(s, d=d, k=k) pgamma(s^k, d)    # Joe (2014, p. 179) "lower bounds"
      Hinv <- function(p, d=d     ) qgamma(p,   d)^d  # Joe (2014, p. 179) "lower bounds"
      if(t == -1) {  # Native to Joe--Ma definition for negative association
        return(      1 - Hcdf(Hinv(1-u, d) + Hinv(1-v, d), d=d, k=k)  )
      } else {       # Rotation 90 degrees counter-clockwise (see COP(..., reflect=4 or "grave") )
        v <- 1 - v   # note manipulation of u but "v -" against the copula formula
        return( u - (1 - Hcdf(Hinv(1-u, d) + Hinv(1-v, d), d=d, k=k)) )
      }
    } else if(d < 0.013) {
      if(t == -1) {  # Native to Joe--Ma definition for negative association
        return(    pmax(u + v - 1, 0))
      } else {       # Rotation 90 degrees counter-clockwise (see COP(..., reflect=4 or "grave") )
        v <- 1 - v   # note manipulation of u but "v -" against the copula formula
        return(u - pmax(u + v - 1, 0))
      }
    }
    if(t == -1) {    # Native to Joe--Ma definition for negative association
      return(      1 - pgamma( (qgamma(1-u, d)^d + qgamma(1-v, d)^d )^k, d)  )
    } else {         # Rotation 90 degrees counter-clockwise (see COP(..., reflect=4 or "grave") )
      v <- 1 - v     # note manipulation of u but "v -" against the copula formula
      return( u - (1 - pgamma( (qgamma(1-u, d)^d + qgamma(1-v, d)^d )^k, d)) )
    }
}


# "JOMAcop.derCOP" <- function(u, v, para=1) { # Joe (2014, p. 178)
#   u <- u[1]
#   d <- para[1]; k <- 1/d
#   x <- qgamma(1-u, d)
#   exp(-(x^d + qgamma(1-v, d)^d)^k ) / exp(-x)
# }

# "JOMAcop.derCOPinv" <- function(u, t, para=1) { # Joe (2014, p. 178)
#   u <- u[1]
#   d <- para[1]; k <- 1/d
#   x <- qgamma(1-u, d)
#   y <- ((x - log(t))^d - x^d)^k
#   1 - pgamma(y, d)
#}
