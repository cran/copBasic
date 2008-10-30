\name{rhoCOP}
\alias{rhoCOP}
\title{ The Spearman's Rho of a Copula }
\description{
Compute the Spearman's Rho \eqn{\rho_\mathbf{C}} of a copula according to Nelson (2006) by
\deqn{\rho_\mathbf{C} = 12\int_0^1 \int_0^1 [\mathbf{C}(u,v) - uv]
                          \mathrm{d}u\mathrm{d}v\mbox{,}}
\deqn{\rho_\mathbf{C} = 12\int_0^1\int_0^1 uv
                          \frac{\delta\mathbf{C}(u,v)}{\delta u}
                          \frac{\delta\mathbf{C}(u,v)}{\delta v}
                          \mathrm{d}u\mathrm{d}v - 3\mbox{, or }}
\deqn{\rho_\mathbf{C} = 12\int_0^1 \int_0^1 \mathbf{C}(u,v) \mathrm{d}u\mathrm{d}v - 3\mbox{.}}
}
\usage{
rhoCOP(cop=NULL, para=NULL, byloops=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{byloops}{Should brute force nested \code{for} loops be used instead of two \code{integrate} functions to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\rho_\mathbf{C}} is returned.
}
\note{
See pp.~167--170, 189, 208 of the reference. Technically, Nelson (2006) also shows that these definitions are a form of call to a \dQuote{concordance} function of two copulas \eqn{Q(\mathbf{C}_1,\mathbf{C}_2)} that involve \eqn{\mathbf{C}_1{=}\mathbf{C}(u,v)} and \eqn{\mathbf{C}_2{=}\mathbf{\Pi}}. As such, in order to keep \code{rhoCOP} a small function, \eqn{\rho_\mathbf{C}} is computed here by a special call to \code{\link{tauCOP}}, which itself, although titled for computation of Kendall's Tau, is equivalent to the concordance function \eqn{Q} (see p.~158--159 of the reference).
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{tauCOP}}
}
\examples{
Rho <- rhoCOP(cop=PSP, para=NULL)
print(Rho)


\dontrun{
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.00395),  para2=c(4.67),
             alpha=0.9392,      beta=0.5699)
Rho <- rhoCOP(cop=composite2COP, para=para)
print(Rho)


para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.14147),  para2=c(20.96),
             alpha=0.0411,      beta=0.6873)
Rho <- rhoCOP(cop=composite2COP, para=para)
print(Rho)

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.10137),  para2=c(4492.87),
             alpha=0.0063,      beta=0.0167)
Rho <- rhoCOP(cop=composite2COP, para=para)
print(Rho)
}

}
\keyword{multivariate}
