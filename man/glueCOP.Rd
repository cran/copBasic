\encoding{utf8}
\name{glueCOP}
\alias{glueCOP}
\title{Gluing Two Copulas}
\description{
The \emph{gluing copula} technique (Erdely, 2017, p. 71), given two bivariate copulas \eqn{\mathbf{C}_A} and \eqn{\mathbf{C}_B} and a fixed value \eqn{0 \le \gamma \le 1} is
\deqn{\mathbf{C}_{\gamma}(u,v) = \gamma\cdot\mathbf{C}_A\bigl(u/\gamma, v\bigr)}
for \eqn{0 \le u \le \gamma} and
\deqn{\mathbf{C}_{\gamma}(u,v) = (1-\gamma)\cdot\mathbf{C}_B\bigl((u-\gamma)
\,/\,(1-\gamma), v\bigr)}
for \eqn{\gamma \le u \le 1} and \eqn{\gamma} represents the \emph{gluing point} in \eqn{u} (horizontal axis). The logic is simply the rescaling of \eqn{\mathbf{C}_A} to \eqn{[0,\gamma] \times [0,1]} and \eqn{\mathbf{C}_B} to \eqn{[\gamma,1] \times [0,1]}. Copula gluing is potentially useful in circumstances for which regression is non-monotone.
}
\usage{
glueCOP(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A special parameter \code{list} (see \bold{Note}) with a mandatory element of \code{glue}  parameter \eqn{\gamma}; and}
  \item{...}{Additional arguments to pass to the copulas.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
The following descriptions list in detail the structure and content of the \code{para} argument:
\describe{
  \item{\code{glue}}{--- The \eqn{\gamma} gluing parameter;}
  \item{\code{cop1}}{--- Function of the first copula \eqn{\mathbf{A}};}
  \item{\code{cop2}}{--- Function of the second copula  \eqn{\mathbf{B}};}
  \item{\code{para1}}{--- Vector of parameters \eqn{\Theta_\mathbf{A}} for  \eqn{\mathbf{A}}; and}
  \item{\code{para2}}{--- Vector of parameters \eqn{\Theta_\mathbf{B}} for  \eqn{\mathbf{B}}.}
}
}
\references{
Erdely, A., 2017, Copula-based piecewise regression (chap. 5)  \emph{in} Copulas and dependence models with applications---Contributions in honor of Roger B. Nelsen, \emph{eds.} Flores, U.M., Amo Artero, E., Durante, F., \enc{Sánchez}{Sanchez}, J.F.: Springer, Cham, Switzerland, ISBN 978--3--319--64220--9, \doi{10.1007/978-3-319-64221-5}.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{breveCOP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, \code{\link{composite3COP}}, \code{\link{convexCOP}}
}
\examples{
\dontrun{
  para <- list(cop1=PLACKETTcop, para1=.2, cop2=GLcop, para2=1.2, glue=0.6)
  densityCOPplot(cop=glueCOP, para=para) # }

\dontrun{
  # Concerning Nelsen (2006, exam. 3.3, pp. 59-61)
  # Concerning Erdely (2017, exam. 5.1, p. 71)
  # Concerning Erdely (2017, exam. 5.2, p. 75)
  # Nelsen's example is a triangle with vertex at [G, 1].
  # Erdley's example permits the construction using glueCOP from M and W.
  "coptri" <- function(u,v, para=NA, ...) {
     p <- para[1]; r <- 1 - (1-p)*v
     if(length(u) > 1 | length(v) > 1) stop("only scalars for this function")
     if(0 <= u & u <= p*v & p*v <= p) {            return(u)
     } else if(  0 <= p*v & p*v <  u & u <  r) {   return(p*v)
     } else if(  p <= r   & r   <= u & u <= 1 ) {  return(u+v-1)
     } else { stop("should not be here in logic") }
  }
  "UsersCop" <- function(u,v, ...) { asCOP(u,v, f=coptri, ...) }
  # Demonstrate Nelsen's triangular copula    (black dots )
  UV <- simCOP(cop=UsersCop, para=0.35, cex=0.5, pch=16)
  # Add Erdley's gluing of M() and W() copula (red circles)
  para <- list(cop1=M, cop2=W, para1=NA, para2=NA, glue=0.35)
  UV <- simCOP(cop=glueCOP,  para=para, col=2,   ploton=FALSE)
  # We see in the plot that the triangular copulas are the same.

  # For G = 0.5, Erdley shows Spearman Rho = 2*G-1 = 0, but
  #  Schweizer-Wolff = G^2 + (G-1)^2 = 0.5, let us check these:
  para <- list(cop1=M, cop2=W, para1=NA, para2=NA, glue=0.5)
  rhoCOP( cop=glueCOP, para=para) # -2.181726e-17
  wolfCOP(cop=glueCOP, para=para) #  0.4999953
  # So, rhoCOP() indicates independence, but wolfCOP() indicates
  # dependence at the minimum value possible for a triangular copula.}

\dontrun{
  x <- seq(0,1, by=0.05); para <- list(cop1=M, cop2=W, para1=NA, para2=NA)
  LCM <- lapply(x, function(i) { para$glue <- i;
                   lcomCOP(cop=glueCOP, para=para, stop.on.error=FALSE) })
  LCM12 <- lapply(LCM, function(i) i$lcomUV)
  LCM21 <- lapply(LCM, function(i) i$lcomVU)
  LCM12 <- t( sapply(LCM12, function(i) i) )
  LCM21 <- t( sapply(LCM21, function(i) i) )
  plot( x, LCM12[,3], ylim=c(-0.8, +0.8), type="l", lty=2,
        xlab="glueCOP() gluing parameter value",
        ylab="L-coskew or L-cokurtosis")
  lines(x, LCM21[,3], col="black")
  lines(x, LCM12[,4])
  lines(x, LCM21[,4], col="darkorchid3")
  rho <- LCM12[,2]
  plot(rho, LCM12[,3], ylim=c(-0.8, +0.8), type="l", lty=2,
        xlab="Spearman Rho", ylab="L-coskew or L-cokurtosis")
  lines(rho, LCM21[,3], col="black")
  lines(rho, LCM12[,4], lty=2)
  lines(rho, LCM21[,4], col="darkorchid3") # }
}
\keyword{copula (formulas)}
\keyword{copula gluing}
\keyword{copula composition}
\keyword{Erdely (2017) Examples and Exercises}

