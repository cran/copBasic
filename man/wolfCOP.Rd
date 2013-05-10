\name{wolfCOP}
\alias{wolfCOP}
\title{ The Schweizer and Wolff's Sigma of a Copula }
\description{
Compute the Schweizer and Wolff's Sigma \eqn{\sigma_\mathbf{C}} of a copula according to Nelson (2006) by
\deqn{\sigma_\mathbf{C} = 12\int_0^1 \int_0^1 |\mathbf{C}(u,v) - uv|\mathrm{d}u\mathrm{d}v\mbox{.}}
}
\usage{
wolfCOP(cop=NULL, para=NULL, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\sigma_\mathbf{C}} is returned.
}
\note{
See p. 209 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{rhoCOP}}
}
\examples{
\dontrun{
wolf <- wolfCOP(cop=PSP)
print(wolf)
rho <- rhoCOP(cop=PSP)
print(rho)
}
}
\keyword{multivariate}
