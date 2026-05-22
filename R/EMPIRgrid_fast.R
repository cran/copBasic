# --------------------------------------------------------------------------------------------------
"EMPIRgrid_fast" <-
function(uv=NULL, ctype=c("1/n", "bernstein"), para=NULL,
         gridonly=FALSE, verbose=FALSE, ...) {
  ctype <- tolower(   ctype ) # Remember that we much lower case it here and then feed that
  ctype <- match.arg( ctype ) # variable to the match.arg(). The reasoning has to do with the
  # way that R processes the argument itself, otherwise and error is issued.

  if(is.null(uv) & ! is.null(para)) uv <- para
  uv <- as.matrix(uv)
  if(ncol(uv) != 2) {
    warning("ncol(uv) != 2, function is only built for bivariate data handling")
    return(NULL)
  }

  n  <- nrow(uv) # sample size
  if(n == 1) {
    warning("sample size n=", nrow(n), " is degenerate for this function")
    return(NULL)
  }

  delprob <- 1/n # default for the 1/n
  # if(ctype == "weibull") { # Weibull plotting position formula
  #  delprob <- 1 / (n+1) # Adaption that is not part of 10.1016/j.amc.2024.128827.
  # }
  # It is not certain how to implement delta probabilities other than 1/n because as the algorithm
  # works, it will not result in recomputed Frechet-Hoeffding limits at the bottom of the columns
  # if say Weibull were used. Does one nudge the limits around? Does one offset the lower limits
  # and leave the upper ending at 1 or the other way around? Does one only include the Bernstein
  # smooth on 1/n? Answer to the later question is that Hernandez-Maldonado, Diaz-Viera, Erdely
  # (2012, doi:10.1016/j.petrol.2012.04.018) and think as do Hernandez-Maldonado, Erdely,
  # Diaz-Viera, and Rios (2024, doi:10.1016/j.amc.2024.128827).

  # We can control whether a t(empCOP) on return is needed or not based on which variable
  # for the algorithm that we treat as the primary and the secondary. These need not have
  # relation to our handling of u,v orientation elsewhere in the package. This choice here
  # can lead to confusion on u,v in the Bernstein loop. Just ignore that confusion because
  # the choice of svncn and pvcn ordering was made solely to avoid the transposition of the
  # matrix on return() to save some CPU time.
  svcn   <- 2 # 10.1016/j.amc.2024.128827 does not reuse these variables but I do so here
  pvcn   <- 1 # 10.1016/j.amc.2024.128827 does not reuse these variables but I do so here
  ID     <- cbind(uv, matrix(data=0, nrow=n, ncol=2))
  ID     <- ID[order(ID[,pvcn]),]
  ID[,4] <- seq_len(n) # sort on the primary first, so that when we sort on the secondary
  ID     <- ID[order(ID[,svcn]),] # that the ID remains in that order, which gets us a
  ID[,3] <- seq_len(n) # cleaner access to pvcoo
  # This change in ordering sequence between the variables differs, but seems required relative
  # to the steps in 10.1016/j.amc.2024.128827.
  #print(ID)

  empCOP <- matrix(data=0, nrow=n+1, ncol=n+1)
  for(j in 1:n) { # adaption of n from n+1 relative to 10.1016/j.amc.2024.128827
    empCOP[j+1, n+1] <- j/n # true Frechet-Hoeffding limits : Regardless of plotting position for the
    empCOP[n+1, j+1] <- j/n # true Frechet-Hoeffding limits : delprob (delta probability) variable.
  }
  # Note, some of the adaption of the indices herein is because authors of 10.1016/j.amc.2024.128827
  # are (were) thinking in a 0-based index language but R is 1-based.
  #for(svid in 1:(n-1)) { # adaption to n-1 from n 10.1016/j.amc.2024.128827
  if(verbose) message("Empirical copula: ")
  for(svid in 2:n) { # adaption to avoid systematic svid+1 on below index access
    if(verbose) message(n+1 - svid,"-", appendLF=FALSE) # tweak index so that sequence ends at "1"
    # V <- matrix(data=NA, nrow=n+1, ncol=n+1)     # CONSOLE VISUALIZATION OF THE COORDINATES
    pvcoo <- ID[svid-1, 4] # coordinate/rank of the primary variable
    # Warning, playing with row indexing is tricky; make sure that all samples in the ID matrix
    # get traversed except(?) the terminal because we always know(?) where the last sample will land?
    # The svid-1 ensure that we start at the top of the ID matrix.
    # V[pvcoo+1, svid] <- 1                        # CONSOLE VISUALIZATION OF THE COORDINATES
    #print(V)                                     # CONSOLE VISUALIZATION OF THE COORDINATES
    #for(j in pvcoo:n) { # j+1s are for 1-based R relative to 0-based C++ of 10.1016/j.amc.2024.128827
    for(j in (pvcoo:n)+1) { # adaption to simplify indexing to avoid j+1 in indices below
      empCOP[j, svid] <- delprob         + empCOP[j, svid-1]
    }
    #for(j in 1:(pvcoo-1)) { # j+1s are for 1-based R relative to 0-based C++ of 10.1016/j.amc.2024.128827
    for(j in 2:pvcoo) { # adaption to simplify indexing to avoid j+1 in indices below
      empCOP[j, svid] <- empCOP[j, svid] + empCOP[j, svid-1]
    }
    #print(empCOP) # CONSOLE VISUALIZATION OF THE BUILDING empCOP
    #cat("-----------------------\n")
  }
  if(verbose) message("done")
  rcnm <- c(0, seq_len(n)/n)
  rownames(empCOP) <- colnames(empCOP) <- rcnm

  if(ctype != "bernstein") { # Bernstein smooth was not required, so return.
    if(gridonly) {
      return(empCOP)        # The grid of the empirical copula.
    } else {
      zzz        <- list()
      zzz$u      <- rcnm    # Vector of the U probabilities with Frechet-Hoeffding limits
      zzz$v      <- zzz$u   # Replicate the U probabilities as the V probabilities
      zzz$empcop <- empCOP  # The grid of the empirical copula.
      zzz$deluv  <- 1/n     # The base deluv, which is the inversion of sample size.
      zzz$ctype  <- ctype
      class(zzz) <- c("empirical.copula.grid", class(zzz))
      return(zzz)
    }
  }

  berCOP <- matrix(data=NA, nrow=n+1, ncol=n+1) # Initialization of a matrix to hold the
  berCOP[    1,] <- empCOP[    1,] # Bernstein smooth with initiation of the edges to
  berCOP[    ,1] <- empCOP[    ,1] # the Frechet-Hoeffding limits of a copula through
  berCOP[(n+1),] <- empCOP[(n+1),] # inheritance from the empCOP itself.
  berCOP[,(n+1)] <- empCOP[,(n+1)] #
  # I chose to have the NA as the value sets so that I could study the build up of the triple
  # sapply depth of the smooth.

  ix <- 2:n           # Interior cells that we will be infilling with the Bernstein smooth.
  nx <- length(  ix ) # Length or rather width of the interior cells.
  sx <- seq_len( nx ) # A sequence length vector to feed the sapply() that does in infill.
  ns <- seq_len( n  ) # A sequence length of the double summations of eq.7 10.1016/j.amc.2024.128827.
  v <- ix/n # We have ability to a priori compute the u, probabilities of the columns and because of
  # the double summation, we pickup some additional efficiency.
  if(verbose) message("Bernstein smooth: ")
  for(i in ix) { # Foreach of the rows in the matrix.
    if(verbose) message(n+1 - i,"-", appendLF=FALSE) # tweak index so that sequence ends at "1"
    u <- rep(i/n, nx) # Create the a priori analog vector of u probabilities.
    berCOP[i, ix] <- sapply(sx, function(k ) { # Row infill.
                 sum(sapply(ns, function(ii) { # A is initialized so second summation sees it.
                                           A <- choose(n, ii) * u[k]^ii * (1 - u[k])^(n-ii)
                 sum(sapply(ns, function(jj) {
                   return(empCOP[ii, jj] * A *  choose(n, jj) * v[k]^jj * (1 - v[k])^(n-jj))
                }))  }))  })
  }
  if(verbose) message("done")

  rownames(berCOP) <- colnames(berCOP) <- rcnm

  if(gridonly) {
    return(berCOP)        # The grid of the empirical copula after Bernstein smooth.
  } else {
    zzz        <- list()
    zzz$u      <- rcnm    # Vector of the U probabilities with Frechet-Hoeffding limits
    zzz$v      <- zzz$u   # Replicate the U probabilities as the V probabilities
    zzz$empcop <- berCOP  # The grid of the empirical copula after Bernstein smooth.
    zzz$deluv  <- 1/n     # The base deluv, which is the inversion of sample size.
    zzz$ctype  <- ctype
    class(zzz) <- c("empirical.copula.grid", class(zzz))
    return(zzz)
  }
}
