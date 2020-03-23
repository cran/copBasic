"RFcop" <- function(u, v, para=NULL, rho=NULL, tau=NULL,
                           fit=c('rho', 'tau'), ...) {
    if(is.null(para)) {
      fit <- match.arg(fit)
      if(is.null(tau) & is.null(rho)) {
        if(fit == "rho") {
          rho <- cor(u,v, method="spearman")
        } else {
          tau <- cor(u,v, method="kendall")
        }
      }
      rt <- NULL
      if(is.null(rho)) {
        try(rt <- uniroot(function(t) { 2*t/(3-t) - tau}, interval=c(0,1)), silent=TRUE)
        if(is.null(rt)) {
          warning("could not uniroot the Theta from the Tau")
          return(NULL)
        }
        para <- rt$root
        names(para) <- "theta"
        names(tau)  <- "Kendall Tau"
        return(list(para=para, tau=tau))
      } else {
        try(rt <- uniroot(function(t) { t*(4-3*t)/(2-t)^2 - rho}, interval=c(0,1)), silent=TRUE)
        if(is.null(rt)) {
          warning("could not uniroot the Theta from the Rho")
          return(NULL)
        }
        para <- rt$root
        names(para) <- "theta"
        names(rho)  <- "Spearman Rho"
        return(list(para=para, rho=rho))
      }
    }
    if(length(para) == 1) {
       if(para < 0 | para > 1) {
         warning("Parameter must be 0 <= Theta <= 1")
         return(NULL)
       }
       tau <- 2*para/(  3-para)
       rho <-   para*(4-3*para)/(2-para)^2
    } else {
       warning("Parameter Theta can not be a vector")
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
    m <- 1 - para; p <- 1 + para; g <- 1:length(u)
    rng <- sapply(g, function(i) range(c(u[i], v[i])))
    mx <- sapply(g, function(i) max(c(u[i],v[i])))
    mn <- sapply(g, function(i) min(c(u[i],v[i]))) # M(u,v)
    cop <- mn + (m/p)*(u*v)^(1/m)*(1 - mx^(-p/m))
    # Test one is more obvious, but test two requires the simulation
    # testing on derCOPinv shown below and commented out.
    if(any(is.nan(cop))) cop[is.nan(cop)] <- mn[is.nan(cop)]
    # Test two. The derCOPinv will end up trying to return the maximum,
    # which seems as mx[! is.finite(cop)] **Note mx** but in simulation
    # that produces little kicks away from M(u,v). It appears the proper
    # interception then is the mn **Note not mx** used in the reassignment below
    if(any(! is.finite(cop))) cop[! is.finite(cop)] <- mn[! is.finite(cop)]
    #print(c(u,v, cop, (u*v)^(1/m), (1 - mx^(-p/m))))
    return(cop)
}

#\note{
# The additive quantity to the right of the
# \eqn{\mathbf{M}(u,v)} in the definition will head to
# \code{NaN} as \eqn{\Theta \rightarrow 1^{-}}. The
# \code{NaN} but the quantity can be replaced with zero,
# which is done in the source code. Testing indicates
# that for large \eqn{\Theta} that \code{uniroot()} in
# \code{\link{derCOPinv}} will occassionally trigger this
# warning message:
#  \preformatted{
#   In uniroot(func, interval = c(0, 1), u = u, LHS = t,
# cop = cop,  ... :
#               NA/Inf replaced by maximum positive value
#  }
# This warning appears harmless. For example, a simulation
# test of \eqn{n=1{,}000} for \eqn{\Theta = 0.99},
# produces 10 warnings. A \eqn{\Theta = 1} itself does
# not because equivalent \code{\link{M}} does not do so
# when it is simulated. Perhaps some subtle algorithmic
# change in \code{RFcop} sources could be made in the
# future.
#}

# The change hinted at is the Test two in the copula,
# for which an example of the trigger is in the comments
# below.

#while(1) { warning(u, " ", t);
# u <- runif(1); t <- runif(1);
# derCOPinv(cop=RFcop, u, t, para=0.999);
# print(last.warning) }

#u <- 0.688409020658582
#t <- 0.508630372118205
#derCOPinv(cop=RFcop, u, t, para=0.999)
