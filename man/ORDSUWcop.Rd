\encoding{utf8}
\name{ORDSUWcop}
\alias{ORDSUWcop}
\title{Ordinal Sums of W-Copula}
\description{
Compute \emph{W-ordinal sum of the summands} (Klement  \emph{et al.}, 2017) within \eqn{\mathcal{I}^2} into \eqn{n} partitions (possibly infinite) within \eqn{\mathcal{I}^2}. Letting \eqn{\mathcal{J}} denote a \emph{partition} of \eqn{\mathcal{I}^2} and \eqn{\mathcal{J}_i = [a_i,\, b_i]} be the \eqn{i}th partition that does not overlap with others and letting also \eqn{\mathbf{C}_i} be a copula for the \eqn{i}th partition, then the \emph{ordinal sum} of these \eqn{\mathbf{C}_i} with parameters \eqn{\Theta_i} \emph{with respect to} \eqn{\mathcal{J}_i} is the copula \eqn{\mathbf{C}} given by

\deqn{\mathbf{C}\bigl(u,v; \mathcal{J}_i, \mathbf{C}_i, \Theta_i, i \in 1,2,\cdots,n\bigr) = a_i + (b_i-a_i)\mathbf{C}_i\biggl(\frac{u-a_i}{b_i-a_i},\, \frac{v-1+b_i}{b_i-a_i};  \Theta_i\biggr)\  \mbox{for}\ (u,v) \in \mathcal{J}^2\mbox{,}}

for points within the partitions, and for points otherwise outside the partitions the coupla is given by

\deqn{\mathbf{C}\bigl(u,v; \mathcal{J}_i, \mathbf{C}_i, i \in 1,2,\cdots,n\bigr) = \mathbf{W}(u,v)\  \mathrm{for}\ (u,v) \ni \mathcal{J}^2\mbox{, and}}

let \eqn{\mathbf{C}_\mathcal{J}(u,v)} be a convenient abbreviation for the copula. (See \code{\link{ORDSUMcop}}, \emph{M-ordinal sum of the summands}.)
}
\usage{
ORDSUWcop(u,v, para=list(cop=M, para=NA, part=c(0,1)), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A list of sublists for the coupla, parameters, and partitions (see \bold{Examples}) and some attempt for intelligent in-fill of \code{para} is made within the sources (the default \code{para} is an example for which \code{cop} and \code{para} elements are converted to lists). The user is responsible that \code{part} element properly canvases by end-point alignment all of \eqn{\mathcal{I}^2}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Klement, E.P., \enc{Kolesárová}{Kolesarova}, A., Mesiar, R., Saminger-Platz, S., 2017, Copula constructions using ultramodularity (chap. 9) \emph{in} Copulas and dependence models with applications---Contributions in honor of Roger B. Nelsen, \emph{eds.} Flores, U.M., Amo Artero, E., Durante, F., \enc{Sánchez}{Sanchez}, J.F.: Springer, Cham, Switzerland, ISBN 978--3--319--64220--9, \doi{10.1007/978-3-319-64221-5}.
}
\author{W.H. Asquith}
\seealso{\code{\link{copBasic-package}}, \code{\link{W_N5p12a}}, \code{\link{ORDSUMcop}}}
\examples{
para <- list(cop=c(CLcop, GHcop), para=list(5, 2), part=c(0,0.25,1)) # break points
UV <- simCOP(n=100, cop=ORDSUMcop, seed=1, para=para, ploton=TRUE, pch=16)
UV <- simCOP(n=100, cop=ORDSUWcop, seed=1, para=para, ploton=FALSE)

\dontrun{
  para <- list(cop=c(CLcop, M, PLcop, GHcop), para=list(4, NA, 0.1, c(3,4)),
              part=list(c(0,0.25), c(0.25,0.35), c(0.35,0.85), c(0.85,1)))
  UV <- simCOP(n=100, cop=ORDSUWcop, para=para, ploton=FALSE)
  plot(c(0,1), c(0,1), xlab="U, NONEXCEEDANCE PROBABILITY", type="n",
                       ylab="V, NONEXCEEDANCE PROBABILITY")
  for(k in seq_len(length(para$part))) {         #  to draw the partitions
    a <- para$part[[k]][1]; b <- para$part[[k]][2]
    polygon(c(a, b, b, a, a), c(1-a,1-a,1-b,1-b,1-a), lty=2, lwd=0.8, col="lightgreen")
    text((a+b)/2, (1-a+1-b)/2, k, cex=3, col="blue") # numbered by partition
  }
  points(UV, pch=21, cex=0.8, col=grey(0.1), bg="white") #}

\dontrun{
  para = list(cop=c(GHcop), para=list(c(2,3)), # internally replicated
              part=list(c(0,0.2), c(0.2,0.3), c(0.3,0.5), c(0.5,0.7), c(0.7,1)))
  UV <- simCOP(n=100, cop=ORDSUWcop, para=para, ploton=FALSE)
  plot(c(0,1), c(0,1), xlab="U, NONEXCEEDANCE PROBABILITY", type="n",
                       ylab="V, NONEXCEEDANCE PROBABILITY")
  for(k in seq_len(length(para$part))) {         #  to draw the partitions
    a <- para$part[[k]][1]; b <- para$part[[k]][2]
    polygon(c(a, b, b, a, a), c(1-a,1-a,1-b,1-b,1-a), lty=2, lwd=0.8, col="lightgreen")
    text((a+b)/2, (1-a+1-b)/2, k, cex=3, col="blue") # numbered by partition
  }
  points(UV, pch=21, cex=0.8, col=grey(0.1), bg="white") #}
}
\keyword{copula (formulas)}
\concept{Ordinal Sums}
\keyword{copula}
\keyword{ordinal sums}
\keyword{W-ordinal sum of the summands}
\concept{summands}
