\name{giniCOP}
\alias{giniCOP}
\title{ The Gini's Gamma of a Copula }
\description{
Compute the Gini's Gamma \eqn{\gamma_\mathbf{C}} of a copula according to Nelson (2006) by

\deqn{\gamma_\mathbf{C} = Q(\mathbf{C},\mathbf{M}) + Q(\mathbf{C},\mathbf{W})\mbox{,}}

where \eqn{\mathbf{C}(u,v)} is the copula, \eqn{\mathbf{M}(u,v)} is \code{\link{M}}, and \eqn{\mathbf{W}(u,v)} is \code{\link{W}}. The function \eqn{Q(a,b)} is a \dQuote{concordance function} (Nelson, 2006, p.~158), which is implemented by a special call to \code{\link{tauCOP}}.

Nelson reports that \dQuote{Gini's Gamma measures a concordance relation of 'distance' between \eqn{\mathbf{C}(u,v)} and monotone dependence, as represented by the copulas \eqn{\mathbf{M}(u,v)} and \eqn{\mathbf{W}(u,v)}.}

The simpler method of computation and the default for the function here, is to compute Gini's \eqn{\gamma_\mathbf{C}} by

\deqn{\gamma_\mathbf{C}  = 4\biggl[\int_0^1 \mathbf{C}(u,u) \mathrm{d}u +  
                             \int_0^1 \mathbf{C}(u,1-u) \mathrm{d}u\biggr] -
                           2\mbox{.}}
This second method is simpler because the single integration is readily deployed (and fast) using two separate calls to the \code{integrate()} function of \R.
}
\usage{
giniCOP(cop=NULL, para=NULL, byQ=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{byQ}{Instead of using the single integrals (p.~181~and~182) and the \code{integrate()} function to compute \eqn{\gamma_\mathbf{C}}, use the concordance function method implemented through \code{\link{tauCOP}};}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the double integration performed by \code{\link{tauCOP}}; and}
  \item{...}{Additional arguments to pass, which are dispatched to the copula function \code{cop} and possibly \code{\link{tauCOP}} if \code{byQ=TRUE}.}
}
\value{
  The value for \eqn{\gamma_\mathbf{C}} is returned.
}
\note{
See p.~180--182 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{tauCOP}}
}
\examples{
gamma1 <- giniCOP(cop=PSP)
print(gamma1)

gamma2 <- giniCOP(cop=PSP, byQ=TRUE)
print(gamma2)
}
\keyword{multivariate}
