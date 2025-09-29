# Gumbel-Hougaard Copula
"GHcop" <-
function(u, v, para=NULL, tau=NULL, tau.big=0.985, cor=NULL, ...) {
  if(! is.null(cor)) tau <- cor

  # tau.big <- 0.985 # tests show at least a failure rate < 1/10000 in simCOP
  # if tau is kept this low, which is a really large tau.
  para.big <- 1/(1-tau.big)

  if(is.null(para)) {
     if(is.null(tau)) {
        if(    length(u)     ==  length(v) &
              (length(u) > 1  &  length(v) > 1)) {
           tau <- cor(u,v, method="kendall")
        } else {
           warning("Argument para (Alpha) is NULL and u an v have unequal length",
                   "that prevents Alpha estimation by Kendall's Tau, returning NULL")
           return(NULL)
        }
        para <- 1/(1-tau)
        if(para < 1) {
           warning("negative correlation can not be fit with this copula, ",
                   "returning NULL")
           return(NULL)
        }
        names(para) <- "theta"; names(tau) <- "tau"
        return(list(para=para, tau=tau))
     } else {
        if(tau < 0) {
           warning("Kendall's Tau is negative, not compatible with this copula, ",
                   "returning NULL")
           return(NULL)
        }
        para <- 1/(1-tau)
        names(para) <- "theta"; names(tau) <- "tau"
        return(list(para=para, tau=tau))
     }
  }

  if(length(para) == 1) {
     if(para[1] < 1) {
        warning("Parameter Theta < 1")
        return(NULL)
     }
     if(para[1] > para.big ) {
        warning("Parameter Theta (para) is big (numerically too big), returning M(u,v)")
        return(M(u,v))
     }
  } else if(length(para) == 2) {
    go <- TRUE
    if(para[1] < 1) {
       warning("Parameter Beta1 is < 1");  go <- FALSE
    }
    if(para[2] <= 0) {
       warning("Parameter Beta2 is <= 0"); go <- FALSE
    }
    if(! go) return(NULL)
  } else if(length(para) == 3) {
     if(para[1] < 1) {
        warning("Parameter Theta (para[1]) is < 1, returning NULL")
        return(NULL)
     }
     if(para[2] < 0 | para[2] > 1) {
        warning("Parameter Pi1 is not in [0, 1], returning NULL")
        return(NULL)
     }
     if(para[3] < 0 | para[3] > 1) {
        warning("Parameter Pi2 is not in [0, 1], returning NULL")
        return(NULL)
     }
  } else {
     stop("Should not be here in logic---too many parameters")
  }


  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ",length(u), " and length v = ",length(v))
    warning("longer object length is not a multiple of shorter object length, ",
            "no recycling")
    return(NA)
  }
  # The extra hassle of vectorization made here is to handle situations
  # in which nested integrals are used where uneven vectors can be passed
  if(length(u) == 1) {
     u <- rep(u, length(v))
  } else if(length(v) == 1) {
     v <- rep(v, length(u))
  }

  if(length(para) == 1) {
     # R's defaults catch Inf correctly so no need to check on the finiteness
     # and make a local copy
     return(exp(-(((-log(u))^para + (-log(v))^para)^(1/para))))
  } else if(length(para) == 2) {
     lo <- .Machine$double.xmin^0.5 # sqrt or just xmin?
     # v <- simCOPmicro(0.01, cop=GHcop2, para=c(2,88))
     # v <- simCOPmicro(0.99, cop=GHcop2, para=c(exp(5),0.1))
     # UV <- simCOP(1000, cop=GHcop2, para=c(exp(2),.0000006)) # still a problem, some V's == 1
     if(para[2] < 1e-6) para[2] <- 1e-6
     b1 <- para[1]; b2 <- -para[2]
     bb1 <- (u^b2 - 1)^b1; bb2 <- (v^b2 - 1)^b1
     cop <- ((bb1 + bb2)^(1/b1) + 1)^(1/b2)
     wnt <- bb1 == Inf | bb2 == Inf # this makes simCOP() work throughout range of parameters
     cop[wnt] <- pmin(u[wnt], v[wnt], na.rm=TRUE) # from extremely large scale lcomCOP() testing.
     # This fixes the "lower" tail relative to reflection. But does not ensure the upper
     # tail goes to M().
     wnt <- bb1 < lo | bb2 < lo # this makes simCOP() work throughout range of parameters
     cop[wnt] <- pmin(u[wnt], v[wnt], na.rm=TRUE)
     return(cop)
  } else if(length(para) == 3) {
     lo <- .Machine$double.eps^0.50; hi <- 1 - lo
     p2 <- para[2]; p3 <- para[3]; d <- para[1]; di <- 1/d
     if(p2 >= hi) p2 <- hi; if(p2 <= lo) p2 <- lo
     if(p3 >= hi) p3 <- hi; if(p3 <= lo) p3 <- lo
     cop <- vector(mode="numeric", length(u))
     domo <- rep(FALSE, length(u))
     for(i in seq_len(length(u))) {
       x   <- -log(u[i]+lo); y <- -log(1)
       cop2u <- exp(-(( (p2*x)^d + (p3*y)^d )^di + (1-p2)*x + (1-p3)*y))
       x   <- -log(u[i])
       cop1u <- exp(-(( (p2*x)^d + (p3*y)^d )^di + (1-p2)*x + (1-p3)*y))
       if(is.nan(cop1u)) cop1u <- 0
       dercopu <- (cop2u - cop1u) / lo
       if(is.nan(dercopu) | dercopu < hi) {
         domo[i] <- TRUE
         #print(c("u", u[i], cop2u, cop1u, dercopu))
         next
       }

       x   <- -log(1); y <- -log(v[i]+lo)
       cop2v <- exp(-(( (p2*x)^d + (p3*y)^d )^di + (1-p2)*x + (1-p3)*y))
                       y <- -log(v[i]   )
       cop1v <- exp(-(( (p2*x)^d + (p3*y)^d )^di + (1-p2)*x + (1-p3)*y))
       if(is.nan(cop1v)) cop1v <- 0
       dercopv <- (cop2v - cop1v) / lo
       # print(c(cop1v, cop2v)
       if(is.nan(dercopv) | dercopv < hi) {
         domo[i] <- TRUE
         #print(c("v", v[i], cop2v, cop1v, dercopv))
         next
       }
       #print( c(cop2u, cop1u, cop2v, cop1v))
       #if(any(c(cop2u, cop1u, cop2v, cop1v) <= 0)) domo[i] == TRUE
     }
     #print(summary(domo))
     #if(d > 80) return(MOcop(u,v, para=c(p2, p3)))
     # UV <- simCOP(1000, cop=GHcop, para=c(800, 0.4166787, 0.7973851))
     # I arrived at the 80 after massive simulations and looking at lowest d such that
     # regardless of the p2, p3, that complete simCOP() appear okay and that the
     # simple numerical derivatives are working. The for() above seeks to check the numerical
     # derivative performance on the margins and if it appear viable that derCOP() would work
     # then the MOcop is not the bail out.

     cop[domo] <- MOcop(u[domo], v[domo], para=c(p2, p3))
     if(sum(domo) == length(u)) return(cop)
     x   <- -log(u[! domo]); y <- -log(v[! domo])
     cr  <- ( (p2*x)^d + (p3*y)^d )^di
     k2  <- (1-p2)*x; k3 <- (1-p3)*y
     cop[! domo] <- exp(-(cr + k2 + k3))
     #print(c(x[! domo], y[! domo], cr[! domo], u[! domo], v[! domo], cop[! domo]))
     #cop[is.nan(cop)] <- 0
     return(cop)
  } else {
     stop("Should not be here in logic")
  }
}


