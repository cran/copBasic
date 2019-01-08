"tEVcop" <- function(u, v, para=NULL, ...) {
   # Implementation Joe (2014, p. 189)
   if(is.null(para)) {
      warning("no parameters, NULL")
   }
   if(length(para) == 2) {
      if(para[1] < -1 | para[1] > +1) {
         warning("Parameter not in -1 <= rho <= +1")
      }
      if(para[2] < 0) {
         warning("Parameter Nu < 0")
         return(NULL)
      }
   } else {
      warning("Parameters must be a vector of rho and nu")
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
   "my.BTev" <- function(w, rho, nu) {
     a <- sqrt((nu+1)/(1-rho^2))
     b <- (w/(1-w))^(1/nu) # note inversion in second term!
     return(w*pt(a*(b-rho), nu+1) + (1-w)*pt(a*(1/b-rho), nu+1))
   }
   x <- -log(u); y <- -log(v) # note minus on -log(v), Joe (2014, p.189) does not
   z <- exp(-(x+y)*my.BTev(x/(x+y), para[1], para[2]))
   z[! is.finite(z)] <- 0
   return(z)
}

# Experimental version shown below based on copula package.
#"tEVcop2" <- function(u, v, para=NULL, ...) {
#   if(is.null(para)) {
#      warning("no parameters, NULL")
#   }
#   if(length(para) == 2) {
#      if(para[1] < -1 | para[1] > +1) {
#         warning("Parameter not in -1 <= rho <= +1")
#      }
#      if(para[2] < 0) {
#         warning("Parameter Nu < 0")
#         return(NULL)
#      }
#   } else {
#      warning("Parameters must be a vector of rho and nu")
#      return(NULL)
#   }
#   if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
#      warning("length u = ", length(u), " and length v = ", length(v))
#      warning("longer object length is not a multiple of shorter object length, ",
#              "no recycling")
#      return(NA)
#   }
#   if(length(u) == 1) {
#      u <- rep(u, length(v))
#   } else if(length(v) == 1) {
#      v <- rep(v, length(u))
#   }
#
#  # https://mediatum.ub.tum.de/doc/1145695/1145695.pdf
#  # https://arxiv.org/pdf/0911.1015.pdf
#  # Derived from copula package (12/05/2018 by WHA)
#  ##' A Pickands dependence function of a bivariate tevCopula
#  ##' w points at which A is to be evaluated
#  "my.ATev" <- function(w, rho, nu) {
#     r2 <- sqrt(1 - rho^2); n2 <- sqrt(nu + 1); cw <- 1-w
#     wnu <-   (w / cw)^(1/nu)
#     x <-     (wnu - rho) / r2 * n2
#     y <- (1 / wnu - rho) / r2 * n2
#     A <- w * pt(x, nu + 1) + cw * pt(y, nu + 1)
#     ifelse(w == 0 | w == 1, 1, A)
#  }
#
#  # Derived from copula::ptevCopula (12/05/2018 by WHA)
#  p <- (r <- uu <- u * v) > 0
#  p <- p & (nna <- ! is.na(p)) # p: positive uu
#  logu    <- log(uu[p])
#  r[  p ] <- exp( logu * my.ATev(log(v[p])/logu, para[1], para[2]))
#  r[! p & nna] <- 0 # when one u is zero
#  return(r)
#}

#para <- c(0.5,6)
#print(tEVcop( 0,0.15, para=para))
#print(tEVcop2(0,0.15, para=para))

#afunc <- function(par, rho=NA, nustar=NA) {
#   par[1] <- pnorm(par[1])
#   par[2] <- exp(par[2])
#   my.rho <- rhoCOP(   tEVcop, par)
#   my.nus <- nustarCOP(tEVcop, par)
#   r <- (rho-my.rho)^2
#   s <- (nustar-my.nus)^2
#   return((r+s))
#}

#UV <- simCOP(1000, cop=GLcop, para=3)
#rho <- cor(UV$U, UV$V, method="spearman")
#nus <- nuskewCOP(para=UV, as.sample=TRUE)
#nut <- nustarCOP(para=UV, as.sample=TRUE)

#para <- optim(par=c(.5, log(5)), fn=afunc, rho=rho, nustar=nut)$par
#para[1] <- pnorm(para[1]); para[2] <- exp(para[2])
#densityCOPplot(cop=GLcop, para=3)
#densityCOPplot(cop=tEVcop, para=para, ploton=FALSE, contour.col=2)
