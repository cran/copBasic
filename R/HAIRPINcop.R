"HAIRPINcop" <-
function(u, v, para=c(1, 2, 1), ...) {
  if(is.null(para)) {
    warning("para can not be NULL to HAIRPINcop")
    return(NULL)
  }
  if(length(para) != 3) {
    warning("para must be length 3 in HAIRPINcop")
    return(NULL)
  }
  a <- as.numeric(para[1])
  p <- as.numeric(para[2])
  reflect <- as.character(as.integer(para[3]))
  if(a < 1) {
    warning("para[1] must be >= 1 in HAIRPINcop")
    return(NULL)
  }
  if(p < 1 | p > 2) {
    warning("para[2] must be 1 <= para[2] <= 2 in HAIRPINcop")
    return(NULL)
  }

  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ",length(u), " and length v = ",length(v))
    warning("longer object length is not a multiple of shorter object length, ",
            "no recycling")
    return(NA)
  }

         if(length(u) == 1) {
    u <- rep(u, length(v))
  } else if(length(v) == 1) {
    v <- rep(v, length(u))
  }

  cdiag <- function(x) a*x^p

  HPcop <- function(myu, myv) {
            sapply(seq_len(length(myu)), function(i) {
               min(c(myu[i], myv[i], mean(cdiag(c(myu[i], myv[i]))) ))
             }) }
  zz <- switch(reflect,
               `1`=            HPcop(    u,     v),
               `2`=u + v - 1 + HPcop(1 - u, 1 - v),
               `3`=    v -     HPcop(1 - u,     v),
               `4`=u -         HPcop(    u, 1 - v))
  return(zz)
}

# Durante, F., Fernandez-Sanchez, J., and Trutschnig, W., 2014, Multivariate copulas with
# hairpin support: Journal of Multivariate Analysis, v. 130, pp. 323-334, \doi{10.1016/j.jmva.2014.06.009}.
