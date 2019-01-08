\encoding{utf8}
\name{PLACKETTcop}
\alias{PLACKETTcop}
\alias{PLcop}
\title{ The Plackett Copula }
\description{
The \emph{Plackett copula} (Nelsen, 2006, pp. 89--92) is
\deqn{\mathbf{C}_\Theta(u,v) = \mathbf{PL}(u,v) = \frac{[1+(\Theta-1)(u+v)]-\sqrt{[1+(\Theta-1)(u+v)]^2 - 4uv\Theta(\Theta-1)}}{2(\Theta - 1)}\mbox{.}}

The Plackett copula (\eqn{\mathbf{PL}(u,v)}) is \emph{comprehensive} because as \eqn{\Theta \rightarrow 0} the copula becomes \eqn{\mathbf{W}(u,v)} (see \code{\link{W}}, \emph{countermonotonicity}), as \eqn{\Theta \rightarrow \infty} the copula becomes \eqn{\mathbf{M}(u,v)} (see \code{\link{M}},  \emph{comonotonicity}) and for \eqn{\Theta = 1} the copula is \eqn{\mathbf{\Pi}(u,v)} (see \code{\link{P}},  \emph{independence}).

Nelsen (2006, p. 90) shows that
\deqn{\Theta = \frac{H(x,y)[1 - F(x) - G(y) + H(x,y)]}{[F(x) - H(x,y)][G(y) - H(x,y)]}\mbox{,}}
where \eqn{F(x)} and \eqn{G(y)} are cumulative distribution function for random variables \eqn{X} and \eqn{Y}, respectively, and \eqn{H(x,y)} is the joint distribution function. Only Plackett copulas have a constant \eqn{\Theta} for any pair \eqn{\{x,y\}}. Hence, Plackett copulas are also known as \emph{constant global cross ratio} or \emph{contingency-type} distributions. The copula therefore is intimately tied to \emph{contingency tables} and in particular the bivariate Plackett defined herein is tied to a \eqn{2\times2} contingency table. Consider the \eqn{2\times 2} contingency table shown at the end of this section, then \eqn{\Theta} is defined as

\deqn{\Theta = \frac{a/c}{b/d} = \frac{\frac{a}{a+c}/\frac{c}{a+c}}{\frac{b}{b+d}/\frac{d}{b+d}}\mbox{\ and\ }\Theta = \frac{a/b}{c/d} = \frac{\frac{a}{a+b}/\frac{b}{a+b}}{\frac{c}{c+d}/\frac{d}{c+d}}\mbox{,}}

where it is obvious that \eqn{\Theta = ad/bc} and \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{d} can be replaced by proporations for a sample of size \eqn{n} by \eqn{a/n}, \eqn{b/n}, \eqn{c/n}, and \eqn{d/n}, respectively. Finally, this copula has been widely used in modeling and as an alternative to bivariate distributions and has respective \emph{lower-} and \emph{upper-tail dependency parameters} of \eqn{\lambda^L = 0} and \eqn{\lambda^U = 0} (\code{\link{taildepCOP}}).

\tabular{rccc}{
\eqn{{-}{-}}     \tab \bold{Low} \tab \bold{High} \tab \bold{Sums}\cr
\bold{Low}  \tab \eqn{a}    \tab \eqn{b}     \tab \eqn{a+b}\cr
\bold{High} \tab \eqn{c}    \tab \eqn{d}     \tab \eqn{c+d}\cr
\bold{Sums} \tab \eqn{a+c}  \tab \eqn{b+d}   \tab \eqn{{-}{-}}
}
}
\usage{
PLACKETTcop(u, v, para=NULL, ...)
PLcop(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
The Plackett copula was the first copula implemented in \pkg{copBasic} as part of initial development of the code base for instructional purposes. Thus, this particular copula has a separate parameter estimation function in \code{\link{PLACKETTpar}}.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTpar}}, \code{\link{PLACKETTsim}}, \code{\link{W}}, \code{\link{M}}, \code{\link{densityCOP}}
}
\examples{
PLACKETTcop(0.4,0.6,para=1)
P(0.4,0.6) # independence copula, same two values because Theta == 1
PLcop(0.4,0.6,para=10.25) # joint probability through positive association

\dontrun{
# Joe (2014, p. 164) shows the closed form copula density of the Plackett.
"dPLACKETTcop" <- function(u,v,para) {
   eta <- para - 1; A <- para*(1 + eta*(u+v-2*u*v))
   B <- ((1 + eta*(u+v))^2 - 4*para*eta*u*v)^(3/2); return(A/B)
}
u <- 0.08; v <- 0.67 # Two probabilities to make numerical evaluations.
del <- 0.0001 # a 'small' differential value of probability
u1 <- u; u2 <- u+del; v1 <- v; v2 <- v+del
# Density following (Nelsen, 2006, p. 10)
dCrect <- (PLcop(u2, v2, para=10.25) - PLcop(u2, v1, para=10.25) -
           PLcop(u1, v2, para=10.25) + PLcop(u1, v1, para=10.25))/del^2
dCanal <- dPLACKETTcop(u,  v,  para=10.25)
dCfunc <- densityCOP(u, v, cop=PLcop, para=10.25, deluv = del)
R <- round(c(dCrect, dCanal, dCfunc), digits=6)
message("Density: ", R[1], "(manual), ", R[2], "(analytical), ", R[3], "(function)");

# Comparison of partial derivatives
dUr <- (PLcop(u2, v2, para=10.25) - PLcop(u1, v2, para=10.25))/del
dVr <- (PLcop(u2, v2, para=10.25) - PLcop(u2, v1, para=10.25))/del
dU  <-  derCOP(u, v, cop=PLcop, para=10.25)
dV  <- derCOP2(u, v, cop=PLcop, para=10.25)
R   <- round(c(dU, dV, dUr, dVr), digits=6)
message("Partial derivatives dU=", R[1], " and dUr=", R[3], "\n",
        "                    dV=", R[2], " and dVr=", R[4]) #}
}
\keyword{Plackett copula}
\keyword{copula (formulas)}
\keyword{copula (density)}
\concept{comprehensive copula}
\keyword{copula}
\keyword{Joe (2014) Examples and Exercises}
