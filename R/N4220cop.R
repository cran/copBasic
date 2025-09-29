"N4220cop" <-
function(u, v, para=NULL, insertM=TRUE, ...) {

    if(length(u) == 1) {
      u <- rep(u, length(v))
    } else if (length(v) == 1) {
      v <- rep(v, length(u))
    }

    TT <- para[1]

    if(is.null(para)) {
       warning("Empty para argument, need value on (0,Inf)")
       return()
    }

    if(TT < 0) {
       warning("Theta < 0, invalid parameter")
       return()
    }
    if(TT <= 0.00001) return(P(u,v))

    if(insertM & TT >= 315.4) return(M(u,v))

    p <- c(0.5, 0.6, 0.7, 0.9, 1.2, 1.5, 1.9, 2.5, 3.1, 3.9, 4.9, 6.2, 7.9, 9.9, 12.5, 15.8,
           19.9, 25, 31.5, 39.7, 49.9, 62.9, 79.2, 99.7, 125.5, 158.1, 199, 250.5, 315.4)
    Uchop <- c(-4.32, -3.95, -3.752, -3.201, -2.615, -2.23, -1.858, -1.458, -1.173, -0.893,
               -0.637, -0.394, -0.162, 0.039, 0.231, 0.413, 0.58, 0.736, 0.885, 1.026, 1.159,
               1.288, 1.409, 1.524, 1.637, 1.743, 1.847, 1.946, 2.043)

    cop <- ( log( exp(u^-TT) + exp(v^-TT) - exp(1) ) )^(-1/TT)

    if(TT < p[1]) return(cop)
    if(insertM) {
      ucop <- pnorm(approx(p, Uchop, xout=TT, rule=2)$y)+0.01
      if(ucop > 1) ucop <- 0.99
      wnt <- u < ucop | v < ucop
      cop[wnt] <- pmin(u[wnt], v[wnt], na.rm=TRUE)
    }
    #print(c(u,v,cop))
    return(cop)
}



# p <- 10^(seq(log10(0.5), 2.5, by=0.1))
# p <- as.integer(p*10)/10
# suppressWarnings(
#   MinU <- sapply(p, function(p) {
#      n <- ifelse(p < 1, 1E5, 1E4)
#      uv <- simCOP(n, cop=N4220cop, para=p, snv=TRUE); mtext(p)
#       t <- round(min(uv$U), digits=3); abline(v=t, col="red"); print(t) }) )
# plot(p, MinU, log="x")


"rN4220cop" <-
function(u, v, para=NULL, insertM=TRUE, ...) {

    if(length(u) == 1) {
      u <- rep(u, length(v))
    } else if (length(v) == 1) {
      v <- rep(v, length(u))
    }

    TT <- para[1]

    if(is.null(para)) {
       warning("Empty para argument, need value on (0,Inf)")
       return()
    }

    if(TT < 0) {
       warning("Theta < 0, invalid parameter")
       return()
    }
    if(TT <= 0.00001) return(P(u,v))

    if(insertM & TT >= 315.4) return(M(u,v))

    p <- c(0.5, 0.6, 0.7, 0.9, 1.2, 1.5, 1.9, 2.5, 3.1, 3.9, 4.9, 6.2, 7.9, 9.9, 12.5, 15.8,
           19.9, 25, 31.5, 39.7, 49.9, 62.9, 79.2, 99.7, 125.5, 158.1, 199, 250.5, 315.4)
    Uchop <- c(-4.32, -3.95, -3.752, -3.201, -2.615, -2.23, -1.858, -1.458, -1.173, -0.893,
               -0.637, -0.394, -0.162, 0.039, 0.231, 0.413, 0.58, 0.736, 0.885, 1.026, 1.159,
               1.288, 1.409, 1.524, 1.637, 1.743, 1.847, 1.946, 2.043)

    cop <- u + v - 1 + ( log( exp((1-u)^-TT) + exp((1-v)^-TT) - exp(1) ) )^(-1/TT)

    if(TT < p[1]) return(cop)
    if(insertM) {
      ucop <- 1-(pnorm(approx(p, Uchop, xout=TT, rule=2)$y)+0.01)
      if(ucop < 0) ucop <- 0.01
      wnt <- u > ucop | v > ucop
      cop[wnt] <- pmin(u[wnt], v[wnt], na.rm=TRUE)
    }
    #print(c(ucop, u,v,cop))
    return(cop)
}

#UV <- simCOP(1000, cop=rN4220cop, para=9.51194)
#level.curvesCOP(rN4220cop, para=9.51194)

#stop()


#b <- 1E4
#  n4220func <- function(k, rho=NA) rho -
#            mean(replicate(10, -3+12*sum(rN4220cop(runif(b),runif(b), para=exp(k)))/b))
#for(rho in seq(0, 0.95, by=0.01)) {

#b <- 1E4
#u <- runif(b)
#RHOPARA <- NULL
#for(p in 6) {#seq(0,100, by=0.1)) {
# v <- simCOPv(u, cop=N4220cop, para=p)
# srho <- cor(u, v, method="spearman")
# trho <- -3+12*mean(replicate(100, sum(N4220cop(runif(b),runif(b), para=p, snapM=FALSE)/b)))
# RHOPARA <- rbind(RHOPARA, data.frame(p, srho, trho))
# print(tail(RHOPARA, n=1))
#}
