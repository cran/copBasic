\name{tauCOP}
\alias{tauCOP}
\title{ The Kendall's Tau of a Copula }
\description{
Compute the Kendall's Tau \eqn{\tau_\mathbf{C}} of a copula according to Nelson (2006) by
\deqn{\tau_\mathbf{C} = 4\int_0^1 \int_0^1
                         \mathbf{C}(u,v)
                         \frac{\delta\mathbf{C}(u,v)}{\delta u}
                         \frac{\delta\mathbf{C}(u,v)}{\delta v}
                         \mathrm{d}u\mathrm{d}v\mbox{.}}

However, Nelson (2006, p. 164) reports that this form is often not amenable to computation and that the expression
\deqn{\tau_\mathbf{C} = 1 - 4\int_0^1 \int_0^1
                         \frac{\delta\mathbf{C}(u,v)}{\delta u}
                         \frac{\delta\mathbf{C}(u,v)}{\delta v}
                         \mathrm{d}u\mathrm{d}v\mbox{.}}
is to be preferred. This expression is used by the \code{tauCOP} function.
}
\usage{
tauCOP( cop=NULL,  para=NULL,
       cop2=NULL, para2=NULL, byloops=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{cop2}{A second copula function;}
  \item{para2}{Vector of parameters or other data structure, if needed, to pass to the second copula;}
  \item{byloops}{Should brute force nested \code{for} loops be used instead of two \code{integrate} functions to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\tau_\mathbf{C}} is returned.
}
\note{
See pp. 158--162, and 164 of the reference.
The function is used in a special call by \code{\link{rhoCOP}} to compute Spearman's Rho of a copula. In this case, \code{cop2=P} of the \eqn{\mathbf{\Pi}} copula and the \code{cop} is set to the copula of interest.  Technically, Nelson (2006) shows that \eqn{\tau_\mathbf{C} = Q(\mathbf{C}_1,\mathbf{C}_2)}, where \eqn{Q} is a \dQuote{concordance} function of two copulas \eqn{Q(\mathbf{C}_1,\mathbf{C}_2)}. Although, titled to reflect \eqn{\tau_\mathbf{C}}, the \code{tauCOP} function when provided the \code{cop2} and \code{para2} arguments functions as the concordance function \eqn{Q} (see p. 158--159 of the reference).
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{rhoCOP}}, \code{\link{derCOP}}, \code{\link{derCOP2}}}
\examples{

Tau <- tauCOP(cop=P, cop2=P)
print(Tau)

\dontrun{

Tau <- tauCOP(cop=M, cop2=M, byloops=TRUE)
print(Tau)

}

Tau <- tauCOP(cop=W, byloops=TRUE)
print(Tau)

\dontrun{

Tau <- tauCOP(cop=P, cop2=P)
print(Tau)

Tau <- tauCOP(cop=M, cop2=M, byloops=TRUE)
print(Tau)

Tau <- tauCOP(cop=W, byloops=TRUE)
print(Tau)

Tau <- tauCOP(cop=W, cop2=P)
print(Tau)

Tau <- tauCOP(cop=P, cop2=W)
print(Tau)

Tau <- tauCOP(cop=P, cop2=M)
print(Tau)


para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.00395),  para2=c(4.67),
             alpha=0.9392,      beta=0.5699)
Tau <- tauCOP(cop=composite2COP,para=para)
print(Tau)


para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.14147),  para2=c(20.96),
             alpha=0.0411,      beta=0.6873)
Tau <- tauCOP(cop=composite2COP,para=para)
print(Tau)




para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.10137),  para2=c(4492.87),
             alpha=0.0063,      beta=0.0167)
Tau <- tauCOP(cop=composite2COP,para=para)
print(Tau)
}

}
\keyword{multivariate}
