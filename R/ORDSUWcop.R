"ORDSUWcop" <-
function(u,v, para=list(cop=M, para=NA, part=c(0,1)), ...) {

  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ",length(u), " and length v = ",length(v))
    warning("longer object length is not a multiple of shorter object length, no recycling in M()")
    return(NA)
  }
  # The extra hassle of vectorization made here is to handle situations
  # in which nested integrals are used where uneven vectors can be passed
  if(length(u) == 1) {
     u <- rep(u, length(v))
  } else if(length(v) == 1) {
     v <- rep(v, length(u))
  }

  if(is.null(para)) {
    para <- list(cop=M, para=NULL, part=c(0,1))
  }

  if(! exists("part", para)) {
    warning("must have the part element of the para list populated")
    return(NULL)
  }

  if(! is.list(para$part)) {
    x  <- as.vector(para$part)
    np <- length(x)
    ck <-   diff(x)
    if(np < 2) {
      warning("para$para given as vector of break pionts but length is not >= 2")
      return(NULL)
    }
    if(any(ck <  0)) {
      warning("para$part given as vector of break points but not monotonic increasing")
      return(NULL)
    }
    if(any(ck == 0)) {
      warning("para$part given as vector of break points but a number is repeated")
      return(NULL)
    }
    J <- lapply(seq(1, np-1, by=1), function(i) c(x[i], x[i+1]))
  } else {
    J <- para$part
  }

  num_partitions <- length(J); kx <- seq_len(num_partitions)
  if(exists("para", para) & is.null(para$para)) {
    Q <- lapply(kx, function(k) NA)
  } else if(exists("para", para) & length(para$para) == 1 && is.na(para$para)) {
    Q <- lapply(kx, function(k) NA)
  } else if(! exists("para", para)) {
    Q <- lapply(kx, function(k) NA)
  } else if(is.list(  para$para) & length(para$para) == 1) {
    Q <- lapply(kx, function(k) para$para[[1]])
  } else if(is.vector(para$para) & length(para$para) == 1) {
    Q <- lapply(kx, function(k) para$para)
  } else {
    Q <- para$para
  }

  if(! exists("cop", para)) {
    C <- lapply(kx, function(k) W)
  } else if(length(para$cop) == 1) {
    if(is.list(para$cop)) {
      C <- lapply(kx, function(k) para$cop[[1]])
    } else {
      C <- lapply(kx, function(k) para$cop)
    }
  } else {
    C <- para$cop
  }

  if(length(J) != length(Q) | length(J) != length(C)) {
    warning("malformed para argument")
    print(c(length(J), length(Q), length(C)))
    return(NULL)
  }

  zz <- sapply(seq_len(length(u)), function(i) {
           for(k in kx) {
             a <- J[[k]][1]; b <- J[[k]][2]; ba <- b-a
             # if(u[i] < a | u[i] > b | v[i] < a | v[i] > b) next # outside [a,b]^2
             if(u[i] <   a | u[i] >=     b) next
             if(v[i] < 1-b | v[i] >= 1 - a) next
             return(a+ba*C[[k]]((u[i]-a)/ba, (v[i]-1+b)/ba, para=Q[[k]], ...))
           }; return(max(u[i]+v[i]-1, 0)) }) # W(u,v) otherwise
  return(zz)
}
