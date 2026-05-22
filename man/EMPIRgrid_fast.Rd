\encoding{utf8}
\name{EMPIRgrid_fast}
\alias{EMPIRgrid_fast}
\title{Fast Grid of the Bivariate Empirical Copula}
\description{
Generate a fast gridded representation of the \emph{bivariate empirical copula} (\enc{Hernández-Maldonado}{Hernandez-Maldonado} \emph{et al.}, 2024). Mathematics for the \code{"1/n"} and \code{"bernstein"} copula are shown under \code{\link{EMPIRcop}}. Herein, the \eqn{\ddot{\mathbf{C}}_n} (double-dots) are used to denote gridding of the copula. This function is a much more recent addition (spring 2026) to the \pkg{copBasic} than its historical ancestor of \code{\link{EMPIRgrid}} because of publication of the algorithm by \enc{Hernández-Maldonado}{Hernandez-Maldonado} \emph{et al.} (2024). The \pkg{copBasic} author personally thanks V.M. \enc{Hernández-Maldonado}{Hernandez-Maldonado} for a May 2026 discussion on the algorithm and some clarifications to the pseudo-code in \enc{Hernández-Maldonado}{Hernandez-Maldonado} \emph{et al.} (2024).
}
\usage{
EMPIRgrid_fast(uv=NULL, ctype=c("1/n", "bernstein"), para=NULL,
               gridonly=FALSE, verbose=FALSE, ...)
}
\arguments{
  \item{uv}{A data frame or matrix of \eqn{u} and \eqn{v} vectors of nonexceedance probabilities (see also \code{para});}
  \item{ctype}{A means to trigger which definition of empirical copula is to be used: \eqn{\mathbf{C}_n(u,v)} (\code{"1/n"}) or \eqn{\mathbf{C}^\mathcal{B}_n(u,v)} (\code{"bernstein"});}
  \item{para}{A matrix of \eqn{u} and \eqn{v} vectors as an alternative to \code{uv} for syntax compatibility for other situations in \pkg{copBasic};}
  \item{verbose}{A logical controlling whether the progress during grid building is to be shown;}
  \item{gridonly}{A logical to return the \code{empcop} component (\eqn{\ddot{\mathbf{C}}_{n}(u,v)} or \eqn{\ddot{\mathbf{C}}^\mathcal{B}_n(u,v)}); and}
  \item{...}{Additional arguments to pass to \code{\link{EMPIRcop}}.}
}
\value{
A \code{matrix} is returned (\eqn{\ddot{\mathbf{C}}_{n}(u,v)} or \eqn{\ddot{\mathbf{C}}^\mathcal{B}_n(u,v)}), if \code{gridonly} is true. Otherwise a \code{list} of the \eqn{u}, \eqn{v}, and \eqn{\ddot{\mathbf{C}}_{n}(u,v)} or \eqn{\ddot{\mathbf{C}}^\mathcal{B}_n(u,v)} is returned. The \code{list} will also contain the \code{deluv} (simply \eqn{1/n} for sample size \eqn{n}) used and the \code{ctype}. The \code{list} is returned as \code{classes} of \code{"empirical.copula.grid"} and \code{"list"}. This is classification might be used in \pkg{copBasic} to check for a grid in other functions. Note, there is no \code{deluv} argument to this function as there is for the historical \code{\link{EMPIRgrid}} of the package.
}
\references{
\enc{Hernández-Maldonado}{Hernandez-Maldonado}, V.M., Erdely, A., \enc{Díaz-Viera}{Diaz-Viera}, M., and Rios, L., 2024, Fast procedure to compute empirical and Bernstein copulas: Applied Mathematics and Computation, v. 477, article 128827, 14 p., \doi{10.1016/j.amc.2024.128827}.
}
\author{W.H. Asquith}
\seealso{\code{\link{EMPIRgrid}}, \code{\link{EMPIRgrid_lkup}}}
\examples{
  para <- list(cop=GHcop, para=3, breve=.1)
  uv <- simCOP(10, cop=breveCOP, para=para)
  gf <- EMPIRgrid_fast(para=uv, gridonly=TRUE)
  gg <- EMPIRgrid(para=uv, gridonly=TRUE, deluv=0.1, ctype="1/n")
  print(sum(gf - gg)) # should be zero

  n <- 5; uv <- matrix(runif(n *2), ncol=2)
  empCOP <- EMPIRgrid_fast(uv, ctype="1/n",       gridonly=TRUE)
            image(empCOP, main="1/n")
  empCOP <- EMPIRgrid_fast(uv, ctype="bernstein", gridonly=TRUE)
            image(empCOP, main="Bernstein")

\dontrun{
  # This permutation asymmetric copula is shown as a means to cross check
  # the orientation of the matrix computations within the function.
  n <- 500; para <- list(cop=GHcop, para=c(12,15), breve=0.5)
  uv <- copBasic::simCOP(n, cop=breveCOP, para=para, graphics=FALSE)
  empCOP <- EMPIRgrid_fast(uv, gridonly=TRUE)
  image(empCOP); points(uv) #}
}
\keyword{empirical copula}
\keyword{empirical copula (utility)}
\keyword{copulatic surface}
\concept{Bernstein copula}
\concept{Bernstein empirical copula}
\keyword{Bernstein polynomial}
\keyword{gridded empirical copula}
