"vuongCOP" <- function(u, v=NULL, cop1=NULL, cop2=NULL, para1=NULL, para2=NULL,
                                  alpha=0.05, method=c("D12", "AIC", "BIC"), ...) {
   method <- match.arg(method)
   if(is.null(v)) {
      if(length(names(u)) != 2) {
         warning("a data.frame having only two columns is required")
         return(NULL)
      } else {
         v <- u[,2]
         u <- u[,1]
      }
   }
   if(length(u) != length(v)) {
      warning("argument(s) or implied arguments u and v are unequal in length, returning NULL")
      return(NULL)
   }
   n <- length(u)
   "Di" <- function(x,y) {
       f1 <- densityCOP(x,y, cop=cop1, para=para1, ...)
       f2 <- densityCOP(x,y, cop=cop2, para=para2, ...)
       tmp <- log(f1/f2)
       return(tmp)
   }
   hatD12 <- sum(sapply(1:n, function(i) {  Di(u[i],v[i])             })) / n
   varD12 <- sum(sapply(1:n, function(i) { (Di(u[i],v[i]) - hatD12)^2 })) / (n-1)
      lo  <-  hi  <- qnorm(1 - alpha/2)*sqrt(varD12)/sqrt(n)
   D12lo  <- hatD12 - lo; D12hi <- hatD12 + hi
   D12    <- data.frame(VuongD.lower=D12lo, VuongD=hatD12 ,VuongD.upper=D12hi,
                     Vuong.sigma=sqrt(varD12), conf.int=(100*(1-alpha)))

   dim1 <- dim(para1)
   dim1 <- ifelse(is.null(dim1), length(para1), sum(dim1))
   dim2 <- dim(para2)
   dim2 <- ifelse(is.null(dim2), length(para2), sum(dim2))
   # AIC and BIC will match if the dimension of the parameter vector/matrix
   # for the two copulas equal each other and thus the difference is zero.
   AICm <- hatD12 -              (dim2 - dim1) / n
   BICm <- hatD12 - (1/2)*log(n)*(dim2 - dim1) / n
   AIClo <- AICm - lo; AIChi <- AICm + hi
   BIClo <- BICm - lo; BIChi <- BICm + hi
   AIC <- data.frame(AIC.lower=AIClo, AIC=AICm, AIC.upper=AIChi, conf.int=(100*(1-alpha)))
   BIC <- data.frame(BIC.lower=BIClo, BIC=BICm, BIC.upper=BIChi, conf.int=(100*(1-alpha)))

   if(method == "D12") {
      CKlo <- D12lo; CKhi <- D12hi
   } else if(method == "AIC") {
      CKlo <- AIClo; CKhi <- AIChi
   } else {
      CKlo <- BIClo; CKhi <- BIChi
   }
   if(CKlo < 0 & CKhi > 0) {
      result <- "Copulas 1 and 2 are not significantly different at 100(1-alpha)"
   } else if(CKhi < 0) {
      result <- "Copula 1 has better fit than Copula 2 at 100(1-alpha)"
   } else if(CKlo > 0) {
      result <- "Copula 2 has better fit than Copula 1 at 100(1-alpha)"
   } else {
      result <- "other result"
   }
   zz <- list(title="Vuong's Procedure for Parametric Copula Comparison",
              result=result, D12=D12, AIC=AIC, BIC=BIC)
   return(zz)
}
