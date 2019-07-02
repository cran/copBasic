\encoding{utf8}
\name{asCOP}
\alias{asCOP}
\title{ A Wrapper on a User-Level Formula to Become a Copula Function }
\description{
This function is intended to document and then to extend a simple API to end users to aid in implementation of other copulas for use within the \pkg{copBasic} package. There is no need or requirement to use \code{asCOP} but for the mathematical definition of some copulas, the \code{asCOP} function might help considerably.  There is a need for special treatment of \eqn{u} and \eqn{v} vectors of probability as each interacts with the vectorization implicit in \R. The special treatment is needed because many copulas are based on the operators such as \code{min()} and \code{max()}. When numerical integration used by the \code{integrate()} function in \R in some copula operators, such as \code{\link{tauCOP}} for the \emph{Kendall Tau} of a copula, special accommodation is needed related to the inherent vectorization in \R and how \code{integrate()} works.

Basically, the problem is that one can not strictly rely in all circumstances on what \R does in terms of value recycling when  \eqn{u} and \eqn{v} are of unequal lengths. The source code is straightforward. Simply put, if lengths of \eqn{u} and \eqn{v} are unity, then there is no concern, and even if the length of \eqn{u} (say) is unity and \eqn{v} is 21, then recycling of \eqn{u} would often be okay. The real danger is when \eqn{u} and \eqn{v} have unequal lengths and those lengths are each greater than unity---\R's treatment can not be universally relied upon with the various numerics herein involving optimization and nested numerical integration.

The example shows how a formula definition of a copula that is not a copula already implemented by \pkg{copBasic} is set into a function \code{deltacop} and then used inside another function \code{UsersCop} that will be the official copula that is compatible with a host of functions in \pkg{copBasic}. The use of \code{asCOP} provides the length check necessary on \eqn{u} and \eqn{v}, and the argument \code{...} provides optional parameter support should the user's formula require more settings.
}
\usage{
asCOP(u, v, f=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{f}{A function for which the user desires to make as a copula; and}
  \item{...}{Additional arguments to pass to the function \code{f} (such as parameters, if needed, for the copula in the form of a list).}
}
\value{
  The value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}}
\examples{
\dontrun{
# Concerning Nelsen (2006, exer. 3.7, pp. 64--65)
"trianglecop" <- function(u,v, para=NULL, ...) {
   # If para is set, then the triangle is rotated 90d clockwise.
   if(! is.null(para) && para == 1) { t <- u; u <- v; v <- t }
   if(length(u) > 1 | length(v) > 1) stop("only scalars for this function")
   v2<-v/2; if(0   <= u    &  u   <= v2 & v2 <= 1/2) { return(u    )
   } else   if(0   <= v2   & v2   <  u  &  u < 1-v2) { return(v2   )
   } else   if(1/2 <= 1-v2 & 1-v2 <= u  &  u <= 1  ) { return(u+v-1)
   } else { stop("should not be here in logic") }
}
"UsersCop" <- function(u,v, ...) { asCOP(u,v, f=trianglecop, ...) }
n=20000; UV <- simCOP(n=n, cop=UsersCop)
# The a-d elements of the problem now follow:
# (a) Pr[V = 1 - |2*U -1|] = 1 and Cov(U,V) = 0; so that two random variables
# can be uncorrelated but each is perfectly predictable from the other
mean(UV$V - (1 - abs(2*UV$U -1))) # near zero; Nelsen says == 0
cov(UV$U, UV$V)                   # near zero; Nelsen says == 0
# (b) Cop(m,n) = Cop(n,m); so that two random variables can be identically
# distributed, uncorrelated, and not exchangeable
EMPIRcop(0.95,0.17, para=UV) # = A
EMPIRcop(0.17,0.95, para=UV) # = B; then A != B
# (c) Pr[V - U > 0] = 2/3; so that two random variables can be identically
# distributed, but their difference need not be symmetric about zero
tmp <- (UV$V - UV$U) > 0
length(tmp[tmp == TRUE])/n # about 2/3; Nelsen says == 2/3
# the prior two lines yield about 1/2 for independence copula P()
# (d) Pr[X + Y > 0] = 2/3; so that uniform random variables on (-1,1) can each
# be symmetric about zero, but their sum need not be.
tmp <- ((2*UV$V - 1) + (2*UV$U - 1)) > 0
length(tmp[tmp == TRUE])/n # about 2/3; Nelsen says == 2/3

# Concerning Nelsen (2006, exam. 3.10, p. 73)
"shufflecop" <- # assume scalar arguments for u and v
function(u,v, para, ...) {
   m <- para$mixer; subcop <- para$subcop
   if(is.na(m) | m <= 0 | m >= 1) stop("m ! in [0,1]")
   if(u <= m) { return(    subcop(1-m+u, v, para=para$para) -
                           subcop(1-m,   v, para=para$para))
   } else {     return(v - subcop(1-m,   v, para=para$para) +
                           subcop(u-m,   v, para=para$para))
   }
}
"UsersCop" <- function(u,v, para=NULL) {
   asCOP(u,v, f=shufflecop, para=para)
}
n <- 1000; u <- runif(n)
para <- list(mixer=runif(1), subcop=W, para=20)
v <- sapply(1:n, function(i) {
      simCOPmicro(u[i], cop=UsersCop, para=para) } )
plot(data.frame(U=u, V=v), pch=17, col=rgb(1,0,1,1),
     xlab="U, NONEXCEEDANCE PROBABILTY", ylab="V, NONEXCEEDANCE PROBABILITY")
mtext("Plackett mixed with a Convex Copula Sum")

#optimize(function(k) { nuskewCOP(cop=UsersCop,
#         para=list(mixer=k, subcop=M)) }, interval=c(0.01,0.99))
# Note (2018-12-28): Now 96 is the nuskewCOP multiplier, but with shuffling of M,
# we can drive the maximum to +/-1.539598, which implies perhaps a multiplier of
# 1/(1.539598/96) of 62.35394. But the 96 inflates the skewness scale more and
# more inline with the scale of nustarCOP() for practical application of using them
# for parameter estimation (see joeskewCOP Note section).

# Concerning Nelsen (2006, exam. 5.14, p. 195)
"deltacop" <- function(u,v, ...) { min(c(u,v,(u^2+v^2)/2))     }
"UsersCop" <- function(u,v, ...) { asCOP(u,v, f=deltacop, ...) }
isCOP.PQD(cop=UsersCop) # TRUE + Rho=0.288 and Tau=0.333 as Nelsen says
isCOP.LTD(cop=UsersCop, wrtV=TRUE) # FALSE as Nelsen says
isCOP.RTI(cop=UsersCop, wrtV=TRUE) # FALSE as Nelsen says}
}
\keyword{copula (utilities)}
\keyword{copula (formulas)}
\keyword{Nelsen (2006) Examples and Exercises}
\keyword{shuffle (in Examples)}
\keyword{triangle (in Examples)}
