"bilmoms" <- function(cop=NULL, para=NULL, only.bilmoms=FALSE,
                      n=1E5, sobol=TRUE, ...) {

   if(is.null(cop)) {
      warning("must have copula argument specified, returning NULL")
      return(NULL)
   }

   if(sobol) {
     if(! exists(".Random.seed")) tmp <- runif(1) # insures definition of .Random.seed
     seed <- sample(.Random.seed, 1)
     uv   <- randtoolbox::sobol(n = n, dim = 2, seed=seed, scrambling=3)
   } else {
     uv <- matrix(data=runif(2*n), ncol=2)
   }

   rho  <- copBasic::rhoCOP(cop=cop, para=para, ...) # d1 = rho/6 (theoretically)

   cuv  <- COP(uv[,1], uv[,2], cop=cop, para=para, ...)

   d1  <- mean(cuv)
   d1  <- 2*d1 - 1/2
   d2  <- mean((                 2*uv[,2]   -  1)*cuv)
   d2  <- 6*d2 - 1/2
   d3  <- mean(( 60*uv[,2]^2 -  60*uv[,2]   + 12)*cuv)
   d3  <-   d3 - 1/2
   d4  <- mean((280*uv[,2]^3 - 420*uv[,2]^2 + 180*uv[,2] - 20)*cuv)
   d4  <-   d4 - 1/2
   err1 <- abs(d1 - rho/6) # The 1/2 is for an unbiased mean
   deltasX1wrtX2 <- c(d1, d2, d3, d4)
   names(deltasX1wrtX2) <- c("BiVarLM:del1[12]", "BiVarLM:del2[12]",
                             "BiVarLM:del3[12]", "BiVarLM:del4[12]")

   d1  <- mean(cuv)
   d1  <- 2*d1 - 1/2
   d2  <- mean((                 2*uv[,1]   -  1)*cuv)
   d2  <- 6*d2 - 1/2
   d3  <- mean(( 60*uv[,1]^2 -  60*uv[,1]   + 12)*cuv)
   d3  <-   d3 - 1/2
   d4  <- mean((280*uv[,1]^3 - 420*uv[,1]^2 + 180*uv[,1] - 20)*cuv)
   d4  <-   d4 - 1/2
   err2 <- abs(d1 - rho/6) # The 1/2 is for an unbiased mean
   deltasX2wrtX1 <- c(d1, d2, d3, d4)
   names(deltasX2wrtX1) <- c("BiVarLM:del1[21]", "BiVarLM:del2[21]",
                             "BiVarLM:del3[21]", "BiVarLM:del4[21]")
   err <- (err1 + err2)/2
   if(only.bilmoms) {
     zz <- list(bilmomUV=deltasX1wrtX2, bilmomVU=deltasX2wrtX1,
                 error.rho=err, source="bilmoms")
     return(zz)
   }

   ulmoms <- lmomco::lmoms(uv[,1], nmom=5)
   vlmoms <- lmomco::lmoms(uv[,2], nmom=5)

   lcX1X2 <- c(NA, deltasX1wrtX2*6) # L-comoments of X1 wrt X2
   lcX2X1 <- c(NA, deltasX2wrtX1*6) # L-comoments of X2 wrt X1

   L1 <- matrix(c(ulmoms$lambdas[1],        NA,        NA, vlmoms$lambdas[1]), ncol=2)
   # The diagonal should be filled with 1/2 if the n is suitable large because 1/2 is
   # the mean of a uniform random variable.
   L2 <- matrix(c(ulmoms$lambdas[2], lcX2X1[2]*ulmoms$lambdas[2],
                                     lcX1X2[2]*vlmoms$lambdas[2],
                  vlmoms$lambdas[2]), ncol=2)
   # The diagonal should be filled with 1/6 if the n is suitable large because 1/6 is
   # the L-scale of a uniform random variable.

   T2 <- matrix(c(                1, lcX2X1[2], lcX1X2[2],                 1), ncol=2)
   T3 <- matrix(c( ulmoms$ratios[3], lcX2X1[3], lcX1X2[3],  vlmoms$ratios[3]), ncol=2)
   T4 <- matrix(c( ulmoms$ratios[4], lcX2X1[4], lcX1X2[4],  vlmoms$ratios[4]), ncol=2)
   T5 <- matrix(c( ulmoms$ratios[5], lcX2X1[5], lcX1X2[5],  vlmoms$ratios[5]), ncol=2)

   zz <- list(bilmomUV=deltasX1wrtX2, bilmomVU=deltasX2wrtX1,
               error.rho=err,
               bilcomoms=list(L1=L1, L2=L2, T2=T2, T3=T3, T4=T4, T5=T5),
               source="bilmoms")
   return(zz)
}
