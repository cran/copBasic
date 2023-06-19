\encoding{utf8}
\name{breveCOP}
\alias{breveCOP}
\title{Add Asymmetry to a Copula}
\description{
Adding \emph{permutation asymmetry} (Chang and Joe, 2020, p. 1596) (\code{\link{isCOP.permsym}}) is simple for a bivariate copula family. Let \eqn{\mathbf{C}} be a copula with respective vectors of parameters \eqn{\Theta_\mathbf{C}}, then the permutation asymmetry is add through an asymmetry parameter \eqn{\beta \in (-1, +1)} for \eqn{-1 \le \beta < 0} by

\deqn{\breve{\mathbf{C}}_{\beta;\Theta}(u,v) = v^{-\beta}\cdot\mathbf{C}(u, v^{(1+\beta)};\Theta)\mbox{, and}}

for \eqn{0 \le \beta \le +1} by

\deqn{\breve{\mathbf{C}}_{\beta;\Theta}(u,v) = u^{+\beta}\cdot\mathbf{C}(u^{(1-\beta)}, v;\Theta)\mbox{.}}
}
\usage{
breveCOP(u,v, para, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A special parameter \code{list} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
The following descriptions list in detail the structure and content of the \code{para} argument where \code{cop1} and \code{cop} and \code{para1} and \code{para} are respectively synonymous to have some structural similarity to the various copula constructors (compositors) of the \pkg{copBasic} package:
\describe{
  \item{\code{beta}}{--- The \eqn{\beta} asymmetry parameter;}
  \item{\code{cop }}{--- Function of the copula \eqn{\mathbf{C}};}
  \item{\code{cop1}}{--- Function of the coupla \eqn{\mathbf{C}};}
  \item{\code{para }}{--- Vector of parameters \eqn{\Theta_\mathbf{C}} for  \eqn{\mathbf{C}}; and}
  \item{\code{para1}}{--- Vector of parameters \eqn{\Theta_\mathbf{C}} for  \eqn{\mathbf{C}}.}
}
}
\references{
Chang, B., and Joe, H., 2020, Copula diagnostics for asymmetries and conditional
dependence: Journal of Applied Statistics, v. 47, no. 9, pp. 1587--1615, \doi{10.1080/02664763.2019.1685080}.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{convex2COP}}, \code{\link{convexCOP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, \code{\link{composite3COP}}, \code{\link{FRECHETcop}}, \code{\link{glueCOP}}}
\examples{
para <- list(beta=0.24, cop1=FRECHETcop, para1=c(0.4, 0.56))
breveCOP(0.87, 0.35, para=para) # 0.282743

\dontrun{
  # Notice the argument cop and para name adjustments to show that
  # translation exists inside the function to have use flexibility.
  para <- list(beta=+0.44, cop1=FRECHETcop, para1=c(0.2, 0.56))
  UV   <- simCOP(1000, cop=breveCOP, para=para)
  para <- list(beta=-0.44, cop= FRECHETcop, para= c(0.2, 0.56))
  UV   <- simCOP(1000, cop=breveCOP, para=para) # }

\dontrun{
  # Testing on a comprehensive copula like the Plackett
  betas <- rhos <- thetas <- brhos <- NULL
  for(beta in seq(-1, 1, by=0.1)) {
    for(rho in seq(-1, 1, by=0.01)) {
       theta  <- PLACKETTpar(rho=rho, byrho=TRUE)
       thetas <- c(thetas, theta)
       para   <- list(beta=beta, cop=PLcop, para=theta)
       brho   <- rhoCOP(cop=breveCOP, para=para)
       betas  <- c(betas, beta); rhos <- c(rhos, rho)
       brhos  <- c(brhos, brho)
    }
  }
  df <- data.frame(beta=betas, theta=thetas, rho=rhos, brho=brhos)
  plot(df$theta, df$brho, log="x", pch=16, cex=0.9, col="seagreen",
       xlab="Plackett parameter", ylab="Spearman Rho")
  lines(df$theta[df$beta == 0], df$brho[df$beta == 0], col="red", lwd=2)
  # Red line is what the Plackett is as its permutation symmetric definition. #}
}
\keyword{copula composition}
\keyword{copula composition (permutation asymmetric)}
\keyword{permutation asymmetry}
