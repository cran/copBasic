\name{hoefCOP}
\alias{hoefCOP}
\title{ The Hoeffding's Phi of a Copula }
\description{
Compute the measure of association known as \emph{Hoeffding's Phi} \eqn{\sigma_\mathbf{C}} of a copula according to Cherunbini \emph{et al.} (2004, p. 164) by
\deqn{\phi_\mathbf{C} = 3 \sqrt{10\int_0^1 \int_0^1 (\mathbf{C}(u,v) - uv)^2\,\mathrm{d}u\mathrm{d}v}\mbox{.}}
}
\usage{
hoefCOP(cop=NULL, para=NULL, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force (\code{brute=TRUE}) integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\phi_\mathbf{C}} is returned.
}
\references{
Cherubini, U., Luciano, E., and Vecchiato, W., 2004, Copula methods in finance: Hoboken, NJ, Wiley, 293 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{rhoCOP}}, \code{\link{wolfCOP}}
}
\examples{
hoefCOP(cop=PSP)
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}

