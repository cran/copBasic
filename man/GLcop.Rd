\encoding{utf8}
\name{GLcop}
\alias{GLcop}
\alias{GLEVcop}
\alias{GLPMcop}
\alias{JOcopBB4}
\title{The Galambos Extreme Value Copula (with Gamma Power Mixture [Joe/BB4] and Lower Extreme Value Limit)}
\description{
The \emph{Galambos copula} (Joe, 2014, p. 174) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{GL}(u,v) = uv\,\mathrm{exp}\bigl[\bigl(x^{-\Theta} + y^{-\Theta}\bigr)^{-1/\Theta}\bigr]\mbox{,}}
where \eqn{\Theta \in [0, \infty)}, \eqn{x = -\log u}, and \eqn{y = -\log v}. As \eqn{\Theta \rightarrow 0^{+}}, the copula limits to \emph{independence} (\eqn{\mathbf{\Pi}}; \code{\link{P}}) and as  \eqn{\Theta \rightarrow \infty}, the copula limits to perfect association (\eqn{\mathbf{M}}; \code{\link{M}}). The Galambos is a \emph{bivariate extreme value copula} (\eqn{BEV}), and parameter estimation for \eqn{\Theta} requires numerical methods.

There are two other genetically related forms. Joe (2014, p. 197) describes an extension of the Galambos copula as a \emph{Galambos gamma power mixture} (GLPM), which is Joe's \emph{BB4 copula}, with the following form
\deqn{\mathbf{C}_{\Theta,\delta}(u,v) = \mathbf{GLPM}(u,v) =
\biggl(x + y - 1 - \bigl[(x - 1)^{-\delta} + (y - 1)^{-\delta} \bigr]^{-1/\delta} \biggr)^{-1/\Theta}\mbox{,}}
where \eqn{x = u^{-\Theta}}, \eqn{y = v^{-\Theta}}, and \eqn{\Theta \ge 0, \delta \ge 0}. (Joe shows \eqn{\delta > 0}, but zero itself seems to work without numerical problems in practical application.) As \eqn{\delta \rightarrow 0^{+}}, the \dQuote{MTCJ family} (Mardia--Takahasi--Cook--Johnson) results (implemented internally with \eqn{\Theta} as the incoming parameter). As \eqn{\Theta \rightarrow 0^{+}}, the Galambos above results with \eqn{\delta} as the incoming parameter.

This Galambos extension in turn has a \emph{lower extreme value limit form} that leads to a \emph{min-stable bivariate exponential} having \emph{Pickand dependence function} of
\deqn{A(x,y; \Theta, \delta) = x + y - \bigl[x^{-\Theta} + y^{-\Theta} - (x^{\Theta\delta} + y^{\Theta\delta})^{-1/\delta} \bigr]^{-1/\Theta}\mbox{,}}
where this third copula is
\deqn{\mathbf{C}^{LEV}_{\Theta,\delta}(u,v) = \mathbf{GLEV}(u,v) =
\mathrm{exp}[-A(-\log u, -\log v; \Theta, \delta)]\mbox{,}}
for \eqn{\Theta \ge 0, \delta \ge 0} and is known as the \emph{two-parameter Galambos}. (Joe shows \eqn{\delta > 0}, but \eqn{\delta = 0} itself seems to work without numerical problems in practical application.)
}
\usage{
GLcop(   u, v, para=NULL, ...)
GLEVcop( u, v, para=NULL, ...)
GLPMcop( u, v, para=NULL, ...) # inserts third parameter automatically
JOcopBB4(u, v, para=NULL, ...) # inserts third parameter automatically
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{To trigger \eqn{\mathbf{GL}(u,v)}, a vector (single element) of \eqn{\Theta}, to trigger \eqn{\mathbf{GLEV}(u,v)}, a two element vector of \eqn{\Theta} and \eqn{\delta} and alias is \code{GLEVcop}, and to trigger \eqn{\mathbf{GLPM}(u,v)}, a three element vector of \eqn{\Theta}, \eqn{\delta}, and any number (the presence of the third entry alone is the triggering mechanism) though aliases \code{GLPM} or \code{JOcopBB4} will insert the third parameter automatically for convenience; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
Joe (2014, p. 198) shows \eqn{\mathbf{GLEV}(u,v; \Theta, \delta)} as a two-parameter Galambos, but its use within the text seemingly is not otherwise obvious. However, testing of the implementation here seems to show that this copula is really not broader in form than \eqn{\mathbf{GL}(u,v; \alpha)}. The \eqn{\alpha} can always(?) be chosen to mimic the \eqn{\{\Theta, \delta\}}. This assertion can be tested from a semi-independent direction. First, define an alternative style of one-parameter Galambos:
\preformatted{
  GL1cop <- function(u,v, para=NULL, ...) {
     GL1pA <- function(x,y,t) { # Pickend dependence func form 1p Galambos
        x + y - (x^-t + y^-t)^(-1/t)
     }
     if(length(u) == 1) { u <- rep(u, length(v)) } else
     if(length(v) == 1) { v <- rep(v, length(u)) }
     exp(-GL1pA(-log(u), -log(v), para[1]))
  }
}
Second, redefine the two-parameter Galambos:
\preformatted{
  GL2cop <- function(u,v, para=NULL, ...) {
     GL2pA <- function(x,y,t,d) { # Pickend dependence func form 2p Galambos
        x + y - (x^-t + y^-t - (x^(t*d) + y^(t*d))^(-1/d))^(-1/t)
     }
     if(length(u) == 1) { u <- rep(u, length(v)) } else
     if(length(v) == 1) { v <- rep(v, length(u)) }
     exp(-GL2pA(-log(u), -log(v), para[1], para[2]))
  }
}
Next, we can combine the Pickend dependence functions into an objective function. This objective function will permit the computation of the \eqn{\alpha} given a pair \eqn{\{\Theta, \delta\}}.
\preformatted{
  objfunc <- function(a,t=NA,d=NA, x=0.7, y=0.7) {
     lhs <- (x^-t + y^-t - (x^(t*d) + y^(t*d))^(-1/d))^(-1/t)
     rhs <- (x^-a + y^-a)^(-1/a); return(rhs - lhs) # to be uniroot'ed
  }
}
A demonstration can now be made:
\preformatted{
  t <- 0.6; d <- 4; lohi <- c(0,100)
  set.seed(3); UV <- simCOP(3000, cop=GL2cop, para=c(t,d), pch=16,col=3,cex=0.5)
  a <- uniroot(objfunc, interval=lohi, t=t, d=d)$root
  set.seed(3); UV <- simCOP(3000, cop=GL1cop, para=a, lwd=0.5, ploton=FALSE)
}
The graphic so produced shows almost perfect overlap in the simulated values. To date, the author has not really found that the two parameters can be chosen such that the one-parameter version can not attain. Extensive numerical experiments using simulated parameter combinations through the use of various copula metrics (tail dependencies, L-comoments, etc) have not found material differences. \bold{Has the author of this package missed something?}
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}, \code{\link{GHcop}}, \code{\link{HRcop}}, \code{\link{tEVcop}}
}
\examples{
# Theta = pi for GLcop and recovery through Blomqvist Beta     (Joe, 2014, p. 175)
log(2)/(log(log(2)/log(1+blomCOP(cop=GLcop, para=pi))))

# Theta = 2 and delta = 3 for the GLPM form and Blomqvist Beta (Joe, 2014, p. 197)
t <- 2; Btheo <- blomCOP(GLPMcop, para=c(t,3))
Bform <- (2^(t+1) - 1 - taildepCOP(GLPMcop, para=c(t,3))$lambdaU*(2^t -1))^(-1/t)
print(c(Btheo, 4*Bform-1)) # [1] 0.8611903 0.8611900

\dontrun{
  # See the Note section but check Blomqvist Beta here:
  blomCOP(cop=GLcop, para=c(6.043619))  # 0.8552863 (2p version)
  blomCOP(cop=GLcop, para=c(5.6, 0.3))  # 0.8552863 (1p version) }
}
\keyword{Galambos copula}
\keyword{Galambos copula (lower extreme value form)}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Galambos extreme value copula}
\keyword{Joe (2014) Examples and Exercises}
\keyword{copula (extreme value)}
\keyword{extreme value copula}
\keyword{Joe/BB4 copula}
\concept{Joe/BB4 copula}
