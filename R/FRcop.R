"FRcop" <- # Frank copula, Joe (2015, p. 165, sec. 4.5.1)
function(u, v, para=NULL, rhotau=NULL, userhotau_chk=TRUE,
               cortype=c("kendall", "spearman", "tau", "rho"), ...) {
  cortype <-   tolower(cortype)
  cortype <- match.arg(cortype)
  D1 <- function(x) { # Joe (2015, p. 166). This integral needed for Kendall's tau.
    integrate(function(t) 1*x^(-1) * (t^1)*(expm1(t))^(-1), lower=0, upper=x,
              subdivisions=200, stop.on.error=FALSE) # expm1(t) = exp(t) - 1
    # set stop.on.error=TRUE for deeper testing when exploring limits of FRcop() implementation
  }
  D2 <- function(x) { # Joe (2015, p. 166). This integral needed for Kendall's tau.
    integrate(function(t) 2*x^(-2) * (t^2)*(expm1(t))^(-1), lower=0, upper=x,
              subdivisions=200, stop.on.error=FALSE) # expm1(t) = exp(t) - 1
    # set stop.on.error=TRUE for deeper testing when exploring limits of FRcop() implementation
  }

  big.lwr <- -3500
  big.upr <- +3500

  para.small <- 1.082793e-06 # from lines of code below and abs() will be used.
  # uniroot(ofunc_tau, interval=c(big.lwr, big.upr), target_tau=0)$root # [1] +1.082793e-06
  # uniroot(ofunc_rho, interval=c(big.lwr, big.upr), target_rho=0)$root # [1] -2.097237e-10

  # it is in the copula computations themselves, so we need another heuristic limit.
  large.para <- 354 # based on copBasic::simCOP behavior and handling of reflection with positive parameters
  large.rho  <- 0.9999479
  large.tau  <- 0.9887531

  if(is.null(para)) {
    if(cortype == "spearman" | cortype == "rho") {
      if(is.null(rhotau)) {
        rho <- cor(u,v, method="spearman")
      } else {
        rho <- rhotau
      }
      ofunc_rho <- function(trial_para, target_rho=NA) {
        rho <- 1 + (12/trial_para) * (D2(trial_para)$value - D1(trial_para)$value) # Joe (2015, p. 166)
        return(target_rho - rho)
      }
      para <- uniroot(ofunc_rho, interval=c(big.lwr, big.upr), target_rho=rho)$root
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
        tau <- 1 + (4/trial_para) * (D1(trial_para)$value - 1) # Joe (2015, p. 166)
        return(target_tau - tau)
      }
      # uniroot(ofunc, interval=c(-3000, +3000), target_tau=0.998)$root
      # Error in integrate(function(t, k = 1) x^(-k) * (t^k) * (exp(t) - 1)^(-1), :
      # the integral is probably divergent
      para <- NULL
      try(para <- uniroot(ofunc_tau, interval=c(big.lwr, big.upr), target_tau=tau)$root)
      if(is.null(para)) {
        try(para <- uniroot(ofunc_tau, interval=c(big.lwr, big.upr), target_tau=tau)$root)
      }
      names(para) <- "theta"
      names(tau ) <- "Kendall Tau"
      return(list(para=para, tau=tau))
    } else {
      stop("should not be here in logic")
    }
  }
  if(length(para) == 1) {
    if(abs(para) < para.small) {
      tau <- 0
    } else {
      if(userhotau_chk) {
        # design choice for lesson is to compute tau again for purposes of outer limits of operation
        # but would be better to accept a precomputed tau just once for given parameter and pass
        # as another argument to this function to avoid the D1() call on the next line.
        d1 <- D1(para)$value
        tau <- 1 + ( 4/para) * (d1             -  1) # Joe (2015, p. 166)
        rho <- 1 + (12/para) * (D2(para)$value - d1) # Joe (2015, p. 166)
        # print(c(rho, tau))
      }
    }
  } else {
    warning("Parameter Theta can not be a vector")
    return(NULL)
  }
  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {   # standard syntax in copBasic style
    warning("length u = ", length(u), " and length v = ", length(v))
    warning("longer object length is not a multiple of shorter object length, ",
            "no recycling")
    return(NA)
  }
  if(length(u) == 1) {                                           # standard syntax in copBasic style
    u <- rep(u, length(v))
  } else if(length(v) == 1) {
    v <- rep(v, length(u))
  }

  if(abs(para[1]) < para.small) return(u*v) # independence

  if(userhotau_chk) {
    if( tau > +large.tau ) return( M(u,v) ) # upper bound copula, perfect 1:+1, limiting condition
    if( tau < -large.tau ) return( W(u,v) ) # lower bound copula, perfect 1:-1, limiting condition
    if( rho > +large.rho ) return( M(u,v) ) # upper bound copula, perfect 1:+1, limiting condition
    if( rho < -large.rho ) return( W(u,v) ) # lower bound copula, perfect 1:-1, limiting condition
  } else {
    if(para > +large.para) return( M(u,v) ) # upper bound copula, perfect 1:+1, limiting condition
    if(para < -large.para) return( W(u,v) ) # lower bound copula, perfect 1:-1, limiting condition
  }
  # Parameter reflection behavior of partial derivative or its inversion by copBasic::derCOP()
  # and copBasic::derCOPinv() for the positive parameters breaks down as log(0) encounter, but the
  # mirror image of the parameter in the negative domains does not see break down, so reverse
  # the parameter for the copula computations but reflect the v.
  p <- para
  if(para > 0) { v <- 1 - v; p <- -p } # reflection PART A, like "grave" operation in copBasic::COP()

  #a <- exp(-p); b <- exp(-p*u); c <- exp(-p*v)         # THE FRANK COPULA
  #cop <- (1 - a - (1 - b)*(1 - c) ) / (1 - a)          # THE FRANK COPULA
  #cop <- -para[1]^(-1) * log(cop)                      # THE FRANK COPULA

  # see ?expm1
  a <- -expm1(-p); b <- -expm1(-p*u); c <- -expm1(-p*v) # THE FRANK COPULA
  cop <- (a - b*c ) / a                                 # THE FRANK COPULA
  lcop <- log(cop)                                      # THE FRANK COPULA
  lcop[lcop == -Inf] <- log(.Machine$double.eps)        # THE FRANK COPULA
  cop <- -p^(-1) * lcop                                 # THE FRANK COPULA
  # The parameter reversing might protect us from the lcop == -Inf meaning that
  # (a - b*c) ---> 0 but let us use one last insurance policy of sorts and make zero
  # become .Machine$double.eps. Likely, real protection is the large.para setting at function top.
  if(para > 0) cop <- u - cop # reflection PART B, like "grave" operation in copBasic::COP()
  return(cop)
}
