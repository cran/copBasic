\encoding{utf8}
\name{M_N5p12b}
\alias{M_N5p12b}
\title{Shuffles of Upper-Bound Copula, Example 5.12b of Nelsen's Book}
\description{
Compute shuffles of \emph{\enc{Fréchet}{Frechet}--Hoeffding upper-bound copula} (Nelsen, 2006, p. 173), which is defined by partitioning \eqn{\mathbf{M}} within \eqn{\mathcal{I}^2} into \eqn{n} subintervals:
\deqn{\mathbf{M}_n(u,v) = \mathrm{min}\biggl(u-\frac{k-1}{n}, v-\frac{n-k}{n} \biggr)}
for points within the partitions
\deqn{(u,v) \in \biggl[\frac{k-1}{n}, \frac{k}{n}\biggr]\times \biggl[ \frac{n-k}{n}, \frac{n-k+1}{n}\biggr]\mbox{,\ }k = 1,2,\cdots,n}
and for points otherwise out side the partitions
\deqn{\mathbf{M}_n(u,v) = \mathrm{max}(u+v-1,0)\mbox{.}}
The support of \eqn{\mathbf{M}_n} consists of \eqn{n} line segments connecting coordinate pairs \eqn{\{(k-1)/n,\, (n-k)/n\}} and \eqn{\{k/n,\, (n-k+1)/n\}} as stated by Nelsen (2006). It is useful that Nelsen stated such as this helps to identify that Nelsen's typesetting of the terms in the second square brackets---the \eqn{V} direction---is reversed from that shown in this documentation. The \emph{Spearman Rho} (\code{\link{rhoCOP}}) is defined by \eqn{\rho_\mathbf{C} = (2/n^2) - 1}, and the \emph{Kendall Tau} (\code{\link{tauCOP}}) by \eqn{\tau_\mathbf{C} = (2/n) - 1}.
}
\usage{
M_N5p12b(u, v, para=1, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A positive integer \eqn{n \in 1, 2, \cdots}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{ORDSUMcop}}, \code{\link{W_N5p12a}}}
\examples{
M_N5p12b(0.4, 0.6, para=3)

\dontrun{
  # Nelsen (2006, exer. 5.12b, p. 173, fig. 5.3b)
  UV <- simCOP(1000, cop=M_N5p12b, para=4) #}
}
\keyword{copula (formulas)}
\keyword{copula (limits/bounds)}
\concept{Shuffle of Frechet upper-bound copula}
\concept{Shuffle of Frechet-Hoeffding upper-bound copula}
\concept{Shuffle of Frechet--Hoeffding upper-bound copula}
\concept{Shuffle of Frechet upper bounds}
\concept{Shuffle of Frechet-Hoeffding upper bounds}
\concept{Shuffle of Frechet--Hoeffding upper bounds}
\keyword{copula}
\keyword{shuffle}
\keyword{Nelsen (2006) Examples and Exercises}

