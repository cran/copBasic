"RFEcop" <- function(u, v, para=NULL, rho=NULL, tau=NULL,
                           fit=c('rho', 'tau'), ...) {

    if(length(para) == 2) {
       if(para[1] < 0 | para[1] > 1) {
         warning("Parameter 1 must be 0 <= Theta[1] <= 1")
         return(NULL)
       }
       if(para[2] < 0 | para[2] > 1) {
         warning("Parameter 2 must be 0 <= Theta[2] <= 1")
         return(NULL)
       }
       #if(para[2] > para[1]) {
       #  warning("Parameter 2 > Parameter 1")
       #  return(NULL)
       #}
    } else {
      warning("para is a vector of length 2: 0 <= para[2] <= para[1] X 1")
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

    a <- 1 / (1 - para[1])
    b <- 1 / (1 - para[2])
    k <- 1 - para[1]*para[2]
    p <- (para[1] - para[2])/para[1]

    g <- seq_len(length(u))
    ru1 <- sapply(g, function(i) rank(c(u[i],v[i])[1] ))  # r(u1) = rank(ui), i = 1,2
    pr1 <- para[ru1]
    #ru2 <- sapply(g, function(i) rank(c(u[i],v[i])[2] )) # r(u2) = rank(ui), i = 1,2
    #pr2 <- para[ru2]

    u12 <- sapply(g, function(i) min(c(u[i], v[i]))) # M(u,v)
    u22 <- sapply(g, function(i) max(c(u[i], v[i])))
    u1a <- u^a; u2b <- v^b
    D <- (para[2]/para[1]) * (1-pr1)^2 / k
    cop <- (para[2]/pr1)*u12 + p*u*u2b + D*u1a*u2b*(1 - (para[1]/pr1) * u22^(-a*b*k) )

    # Test one is more obvious, but test two requires the simulation
    # testing on derCOPinv shown below and commented out.
    #if(any(is.nan(cop))) cop[is.nan(cop)] <- mn[is.nan(cop)]
    # Test two. The derCOPinv will end up trying to return the maximum,
    # which seems as mx[! is.finite(cop)] **Note mx** but in simulation
    # that produces little kicks away from M(u,v). It appears the proper
    # interception then is the mn **Note not mx** used in the reassignment below
   # if(any(! is.finite(cop))) cop[! is.finite(cop)] <- mn[! is.finite(cop)]
    #print(c(u,v, cop, (u*v)^(1/m), (1 - mx^(-p/m))))
    return(cop)
}
# Saali, T., Mesfioui, M., and Shabri, A., 2024, A novel multivariate copula of Raftery type with multiple dependence parameters and its neutrosophic application in finance: International Journal of Neutrosophic Science, v. 23, no. 2, pp. 296--307, \doi{10.54216/IJNS.230224}.
# saali@graduate.utm.my

#layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
#para <- c(0.3, 0.1)
#uv <- simCOP(200, cop=RFEcop, para=para)
#mtext(paste0("(", paste(para, collapse=", "), ")"))
#para <- c(0.9, 0.7)
#uv <- simCOP(200, cop=RFEcop, para=para)
#mtext(paste0("(", paste(para, collapse=", "), ")"))
#para <- c(0.7, 0.9)
#uv <- simCOP(200, cop=RFEcop, para=para)
#mtext(paste0("(", paste(para, collapse=", "), ")"))
#para <- c(0.95, 0.95)
#uv <- simCOP(200, cop=RFEcop, para=para)
#mtext(paste0("(", paste(para, collapse=", "), ")"))

#para <- c(0.3, 0.1)
#RFEcop(0.5, 0.5, para=para)
#para <- c(0.9, 0.7)
#RFEcop(0.5, 0.5, para=para)
#para <- c(0.7, 0.9)
#RFEcop(0.5, 0.5, para=para)
#para <- c(0.95, 0.95)
#RFEcop(0.5, 0.5, para=para)
