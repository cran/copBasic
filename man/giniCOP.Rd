\name{giniCOP}
\alias{giniCOP}
\title{ The Gini's Gamma of a Copula }
\description{
Compute the measure of association known as \emph{Gini's Gamma} \eqn{\gamma_\mathbf{C}} (Nelsen, 2006, pp. 180--182), which is defined as
\deqn{\gamma_\mathbf{C} = Q(\mathbf{C},\mathbf{M}) + Q(\mathbf{C},\mathbf{W})\mbox{,}}

where \eqn{\mathbf{C}(u,v)} is the copula, \eqn{\mathbf{M}(u,v)} is the \code{\link{M}} function, and \eqn{\mathbf{W}(u,v)} is the \code{\link{W}} function. The function \eqn{Q(a,b)} is a \dQuote{concordance function} (Nelsen, 2006, p. 158), which is implemented by a specially formed call to \code{\link{tauCOP}}. Nelsen also reports that \dQuote{Gini's Gamma measures a concordance relation of \dQuote{distance} between \eqn{\mathbf{C}(u,v)} and monotone dependence, as represented by the copulas \eqn{\mathbf{M}(u,v)} and \eqn{\mathbf{W}(u,v)}.}

A simpler method of computation and the default for \code{giniCOP} is to compute Gini's \eqn{\gamma_\mathbf{C}} by

\deqn{\gamma_\mathbf{C}  = 4\biggl[\int_\mathcal{I} \mathbf{C}(u,u)\,\mathrm{d}u +
                             \int_\mathcal{I} \mathbf{C}(u,1-u)\,\mathrm{d}u\biggr] -
                           2\mbox{,}}
or in terms of the primary \eqn{\delta(t)} and secondary \eqn{\delta^\star(t)} diagonals (see \code{\link{diagCOP}}) by
\deqn{\gamma_\mathbf{C}  = 4\biggl[\int_\mathcal{I} \mathbf{\delta}(t)\,\mathrm{d}t +
                             \int_\mathcal{I} \mathbf{\delta^\star
                             }(t)\,\mathrm{d}t\biggr] -
                           2\mbox{.}}


This simpler method is more readily implemented because the single unnested integration are readily deployed (and fast) and accommodated using the \code{integrate()} function of \R. The \eqn{\gamma_\mathbf{C}} is based on integration of the \emph{primary diagonal} and the \emph{secondary diagonal} of the copula (see \code{\link{diagCOP}}).
}
\usage{
giniCOP(cop=NULL, para=NULL, by.concordance=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{by.concordance}{Instead of using the single integrals (Nelsen, 2006, pp. 181--182) to compute \eqn{\gamma_\mathbf{C}}, use the concordance function method implemented through \code{\link{tauCOP}}; and}
  \item{...}{Additional arguments to pass, which are dispatched to the copula function \code{cop} and possibly \code{\link{tauCOP}} if \code{by.concordance=TRUE}, such as \code{delta} used by that function.}
}
\value{
  The value for \eqn{\gamma_\mathbf{C}} is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}}
}
\examples{
giniCOP(cop=PSP)
\dontrun{
giniCOP(cop=PSP, by.concordance=TRUE)
}
\dontrun{
n <- 9000; set.seed(56)
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, para1=1.45, para2=21.9,
             alpha=0.41, beta=0.08)
D <- simCOP(n=n, cop=composite2COP, para=para, cex=0.5, col=rgb(0,0,0,0.2), pch=16)
tauCOP(cop=composite2COP, para=para)  # 0.3806909
cor(D$U, D$V, method="kendall")       # 0.3789918
blomCOP(cop=composite2COP, para=para) # 0.4037908
giniCOP(cop=composite2COP, para=para) # 0.4334687
GINI <- sum(abs(rank(D$U)+rank(D$V)-n-1)) - sum(abs(rank(D$U)-rank(D$V)))
print(GINI/as.integer(n^2/2))         # 0.4287894
rhoCOP(cop=composite2COP, para=para)  # 0.5257662
cor(D$U, D$V, method="spearman")      # 0.5236753
lmomco::lcomoms2(D)$T2      # 1.0000000 0.5239816
                            # 0.5251972 1.0000000
}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
