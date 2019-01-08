"GLcop" <- function(u,v, para=NULL, ...) {
  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ",length(u), " and length v = ",length(v))
    warning("longer object length is not a multiple of shorter object length, no recycling")
    return(NA)
  }
  # The extra hassle of vectorization made here is to handle situations
  # in which nested integrals are used where uneven vectors can be passed
  if(length(u) == 1) {
     u <- rep(u, length(v))
  } else if(length(v) == 1) {
     v <- rep(v, length(u))
  }

  if(length(para) == 1) { # Joe (2014, p. 174)
    if(para[1] < 0) {
      warning("parameter must be 0 <= theta < infinity [well numerically 100]")
      return(NA)
    }
    if(para[1] < .Machine$double.eps) return(P(u,v))
    #x <- seq(80,110,by=.1)
    #y <- sapply(x, function(t) rhoCOP(cop=GLcop, para=t))
    #plot(x,y); lines(c(94,94),c(0,1))
    if(para[1] > 94) return(M(u,v)) # 94 determined by rhoCOP experiments
    para[1] <- -para[1]
    suppressWarnings(cop <- u * v * exp( ( (-log(u))^para[1] +
                                           (-log(v))^para[1] )^(1/para[1]) ))
    #cop[is.nan(cop)] <- 0
    para[1] <- -para[1]  # Note switch back to original sign convention
    if(para[1] > 50) { #rint(c(u,v,cop))
      wnt <- u > 0.99 & v > 0.99
      if(any(wnt)) cop[wnt] <- M(u[wnt],v[wnt])
    } else if(para[1] > 35) {
      wnt <- u > 0.995 & v > 0.995
      if(any(wnt)) cop[wnt] <- M(u[wnt],v[wnt])
    } else if(para[1] > 25) {
      wnt <- u > 0.999 & v > 0.999
      if(any(wnt)) cop[wnt] <- M(u[wnt],v[wnt])
    } else if(para[1] > 15) {
      wnt <- u > 0.9998 & v > 0.9998
      if(any(wnt)) cop[wnt] <- M(u[wnt],v[wnt])
    }
    return(cop) ################ END GALAMBOS
  } else if(length(para) == 2) { # Joe (2014, p. 198) COP_LEV
    if(para[1] < 0) {
      warning("theta must be 0 < theta < infinity")
      return(NA)
    }
    if(para[2] < 0) {
      warning("delta must be 0 <= delta < infinity")
      return(NA)
    }
    if(para[1] < .Machine$double.eps^0.5) return(P(u,v))
    # Min-Stable Bivariate Exponential Family, a two-parameter Galambos
    if(para[2] > exp(4.75049-0.89850*log(para[1])-0.03948*log(para[1])^2)) {
       return(M(u,v))
    }
    Afunc <- function(x,y,t,d) { # Joe (2014, p. 198)
      x + y - (x^-t + y^-t - (x^(t*d) + y^(t*d))^(-1/d))^(-1/t)
    }
    cop <- exp(-Afunc(-log(u), -log(v), para[1],para[2])) # Joe (2014, p. 198)
    cop[is.nan(cop)] <- 0 # Does this need to be more sophisticated
    # in the case for the Gamma Power Mixture of Galambos?????
    return(cop) ################ END COP_LEV
  } else if(length(para) == 3) { # Joe (2014, p. 197)
    if(para[1] < 0) {
      warning("theta must be 0 < theta < infinity")
      return(NA)
    }
    if(para[2] < 0) {
      warning("delta must be 0 < delta < infinity")
      return(NA)
    }
    # Note para[3] is not used, just a triggering mechanism.
    # Testing indicates that these tests might not be needed.
    if(para[1] <= .Machine$double.eps^0.25) { # test needed though?
       # Galambos copula of para[2] results for small para[1]
       return(GLcop(u,v, para=para[2]))
    }
    if(para[2] <= .Machine$double.eps^0.25) { # test needed though?
       # MTCJ copula of para[1] for small para[2]
       return((u^-para[1] + v^-para[1] -1)^(-1/para[1]))
    }
    # This next test is very complicated. A nested theta=1:100, delta=1:100
    # was done and rhoCOP run on the combinations as the parameters get
    # large, failures occur in that rhoCOP starts trending down from 0.999..
    # into negatives. WHA then found the curve of maximum rhos within the
    # matrix and isolated the maximums and then ran a log-log regression
    # that is shown below. For parameters to the upper left of this regression
    # the M() copula is returned, this avoid numerical problems.
    # This approach is confirmed in part by watching simCOP at large
    # sample sizes and noting the occasional spikes from near M().
    if(para[2] > exp(4.88376-0.69985*log(para[1])-0.05946*log(para[1])^2)) {
       return(M(u,v))
    }
    # To do, a small para[1] and large para[2] can still leak through and
    # the numerical errors occur deep in the upper right corner. M() should be
    # triggered, but how?

    # Continuing ....
    para[1] <- -para[1]; para[2] <- -para[2]
    uo <- u; vo <- v
    u <- u^para[1]; v <- v^para[1]
    cop <- (u + v - 1 - ( (u-1)^para[2] +
                          (v-1)^para[2] )^(1/para[2])
           )^(1/para[1])
    if(any(is.nan(cop))) { # Some degenerate behavior away from
      # u=0 or v=0 occurs in the lower corner, a leg will kick off from M()
      # So here, we are intercepting the kick via leaving the cop as NaN
      # and then resetting to M() (the minimum)
      cop[is.nan(cop)] <- min(c(uo[is.nan(cop)], vo[is.nan(cop)]))
      # This is.nan treatment differs from that for the Galambos in the
      # length(para[1]) [the regular 1-parameter Galambos]
    }
    # The various "large" para[2] checks below were developed after
    # the log-log regression shown above. Still have some numerical
    # issues right up near the upper-left corner. These tests then
    # copied to the Galambos too during testing. n=10,000 sample sizes
    # These are partly protecting from degeneracy at the boundary as
    # mentioned by Joe (2014, p. 198).
    para[2] <- -para[2] # Note switch back to original sign convention
    if(para[2] > 50) {
      wnt <- uo > 0.99 & vo > 0.99
      if(any(wnt)) cop[wnt] <- M(uo[wnt],vo[wnt])
    } else if(para[2] > 35) {
      wnt <- uo > 0.995 & vo > 0.995
      if(any(wnt)) cop[wnt] <- M(uo[wnt],vo[wnt])
    } else if(para[2] > 25) {
      wnt <- uo > 0.999 & vo > 0.999;
      if(any(wnt)) cop[wnt] <- M(uo[wnt],vo[wnt])
    } else if(para[1] > 15) {
      wnt <- u > 0.9998 & v > 0.9998
      if(any(wnt)) cop[wnt] <- M(uo[wnt],vo[wnt])
    }
    return(cop) ################ END GAMMA POWER MIXTURE
  } else {
    stop("only one, two, or two with third trigger parameters in GLcop supported")
  }
}

"JOcopBB4" <- function(u,v, para=NULL, ...) {
  if(length(para) != 2) {
    warning("GLPMcop/JOcopBB4 through GLcop requires two parameters, ",
            "third trigger is automatic")
    return(NULL)
  }
  GLcop(u,v, para=c(para,1), ...)
}

"GLPMcop" <- function(u,v, para=NULL, ...) {
  if(length(para) != 2) {
    warning("GLPMcop/JOcopBB4 through GLcop requires two parameters, ",
            "third trigger is automatic")
    return(NULL)
  }
  GLcop(u,v, para=c(para,1), ...)
}

"GLEVcop" <- function(u,v, para=NULL, ...) {
  if(length(para) != 2) {
    warning("Lower extreme value limit through GLcop requires two parameters")
    return(NULL)
  }
  GLcop(u,v, para=para, ...)
}

############### LOWER EXTREME VALUE LIMIT OF THE GALAMBOS ########
#h <- matrix(nrow =length(1:100), ncol=length(1:100))
#for(i in 1:100) {
# for(j in 1:100) {
#   message(i, " ", j)
#   h[i,j] <- rhoCOP(cop=GLcop, para=c(i,j))
# }
#}
#contour(h, levels=c(0.999))
#image(h)
#k <- 0
#I <- J <- NA
#for(i in 1:100) {
# here <- (1:100)[h[i,] == max(h[i,], na.rm=TRUE)]
# here <- here[! is.na(here)]
# if(length(here) == 0) next
# print(c(i,here, i+here))
# I[k] <- i; J[k] <- here; k <- k + 1
# points(i/100, here/100, col=2, cex=0.5)
#}
#II <- seq(0.1,200, by=.1)
#plot(c(.1,1000), c(0.1,1000), type="n", log="xy")
#points(I,J)
#LM <- lm(log(J)~log(I)+I(log(I)^2))
#points(I, exp(fitted.values(LM)), col=3)
#lines(II, exp(4.75049-0.89850*log(II)-0.03948*log(II)^2), col=4)
##################################################################


############### GAMMA POWER MIXTURE OF THE GALAMBOS ##############
# The code below was used to make the regression equation
# implemented for joint protection on large parameters for the

#h <- matrix(nrow =length(1:100), ncol=length(1:100))
#for(i in 1:100) {
# for(j in 1:100) {
#   message(i, " ", j)
#   h[i,j] <- rhoCOP(cop=GLcop, para=c(i,j,1))
# }
#}
#contour(h, nlevels=20)
#contour(h, levels=c(0.999))
#diag(h) # looking for 0.999
#(1:100)[diag(h) == max(diag(h), na.rm=TRUE)]
#UV <- simCOP(1000, cop=GLcop, para=c(13,13))
#contour(h, levels=c(0.999))
#k <- 0
#I <- J <- NA
#for(i in 1:100) {
# here <- (1:100)[h[i,] == max(h[i,], na.rm=TRUE)]
# here <- here[! is.na(here)]
# if(length(here) == 0) next
# print(c(i,here, i+here))
# I[k] <- i; J[k] <- here; k <- k + 1
# points(i/100, here/100, col=2, cex=0.5)
#}
#for(i in 1:100) {
# here <- (1:100)[h[,i] == max(h[,i], na.rm=TRUE)]
# here <- here[! is.na(here)]
# if(length(here) == 0) next
# print(c(i,here, i+here))
# I[k] <- i; J[k] <- here; k <- k + 1
# points(i/100, here/100, col=3)
#}
#II <- seq(0.1,200, by=.1)
#plot(c(.1,1000), c(0.1,1000), type="n", log="xy")
#points(I,J)
#LM <- lm(log(J)~log(I)+I(log(I)^2))
#points(I, exp(fitted.values(LM)), col=3)
#lines(II, exp(4.88376-0.69985*log(II)-0.05946*log(II)^2), col=4)
##################################################################




