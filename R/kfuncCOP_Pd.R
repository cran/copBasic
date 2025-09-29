"kfuncCOP_Pd" <- function(z, d=2) {
  f <- sapply(z, function(f) {
         f + f*sum(sapply(1:(d-1), function(k) ((-log(f))^k) / factorial(k))) })
  f[is.nan(f)] <- 0
  return(f)
}
