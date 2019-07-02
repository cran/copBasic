\encoding{utf8}
\name{JOcopB5}
\alias{JOcopB5}
\title{ The Joe/B5 Copula (B5)}
\description{
The \emph{Joe/B5 copula} (Joe, 2014, p. 170) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{B5}(u,v) = 1 - \bigl[(1-u)^\Theta + (1-v)^\Theta - (1-u)^\Theta (1-v)^\Theta\bigr)\mbox{,}}
where \eqn{\Theta \in [1,\infty)}.
The copula as \eqn{\Theta \rightarrow \infty} limits to the \emph{comonotonicity coupla} (\eqn{\mathbf{M}(u,v)} and \code{\link{M}}), as \eqn{\Theta \rightarrow 1^{+}} limits to \emph{independence copula} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}). Finally, the parameter \eqn{\Theta} is readily computed from a \emph{Kendall Tau} (\code{\link{tauCOP}}) by
\deqn{\tau_\mathbf{C} = 1 + \frac{2}{2-\Theta}\bigl(\psi(2) - \psi(1 + 2/\Theta)\bigr)\mbox{,}}
where \eqn{\psi} is the \code{digamma()} function and as \eqn{\Theta \rightarrow 2} then \deqn{\tau_\mathbf{C}(\Theta \rightarrow 2) = 1 - \psi'(2)} where \eqn{\psi'} is the \code{trigamma()} function.
}
\usage{
JOcopB5(u, v, para=NULL, tau=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{tau}{Optional Kendall Tau; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned. Otherwise if \code{tau} is given, then the \eqn{\Theta} is computed and a \code{list} having
  \item{para}{The parameter \eqn{\Theta}, and}
  \item{tau}{Kendall tau.}
and if \code{para=NULL} and \code{tau=NULL}, then the values within \code{u} and \code{v} are used to compute Kendall Tau and then compute the parameter, and these are returned in the aforementioned list.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}
}
\examples{
# Upper tail dependency of Theta = pi --> 2 - 2^(1/pi) = 0.753131 (Joe, 2014, p. 171).
taildepCOP(cop=JOcopB5, para=pi)$lambdaU # 0.75313

# Blomqvist Beta of Theta = pi (Joe, 2014, p. 171).
blomCOP(cop=JOcopB5, para=pi)        # 0.5521328
3 - 4*(2*(1/2)^pi - (1/4)^pi)^(1/pi) # 0.5521328

\dontrun{
# A test near the limiting Theta for trigamma()
UV <- simCOP(cop=JOcopB5, para=2, n=10000)
para <- JOcopB5(UV[,1], UV[,2])$para
message("Tau difference ", round(2-para, digits=2), " is small.") #}
}
\keyword{Joe/B5 copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Joe/B5 copula}
\keyword{Joe (2014) Examples and Exercises}
