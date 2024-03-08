\encoding{utf8}
\name{breveCOP}
\alias{breveCOP}
\title{Add Asymmetry to a Copula}
\description{
Adding \emph{permutation asymmetry} (Chang and Joe, 2020, p. 1596) (\code{\link{isCOP.permsym}}) is simple for a bivariate copula family. Let \eqn{\mathbf{C}} be a copula with respective vectors of parameters \eqn{\Theta_\mathbf{C}}, then the permutation asymmetry is added through an asymmetry parameter \eqn{\beta \in (-1, +1)} for \eqn{-1 \le \beta < 0} by

\deqn{\breve{\mathbf{C}}_{\beta;\Theta}(u,v) = v^{-\beta}\cdot\mathbf{C}(u, v^{(1+\beta)};\Theta)\mbox{, and}}

for \eqn{0 \le \beta \le +1} by

\deqn{\breve{\mathbf{C}}_{\beta;\Theta}(u,v) = u^{+\beta}\cdot\mathbf{C}(u^{(1-\beta)}, v;\Theta)\mbox{.}}
The parameter \eqn{\beta} clashes in name and symbology with a parameter used by functions \code{\link{composite1COP}}, \code{\link{composite2COP}}, and \code{\link{composite3COP}}. As a result, support for alternative naming is provided for compatibility.
}
\usage{
breveCOP(u,v, para, breve=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A special parameter \code{list} (see \bold{Note});}
  \item{breve}{An alternative way from \code{para} to set the \eqn{\beta} for this function; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
The following descriptions list in detail the structure and content of the \code{para} argument where \code{cop1} and \code{cop} and \code{para1} and \code{para} are respectively synonymous to have some structural similarity to the various copula constructors (compositors) of the \pkg{copBasic} package:
\describe{
  \item{\code{beta}}{--- The \eqn{\beta} asymmetry parameter;}
  \item{\code{breve}}{--- The \eqn{\beta} asymmetry parameter and presence of \code{breve} will not cause the non-use of \code{beta}; this feature is present so that \code{beta} remains accessible to the compositors that use \code{beta} (see \bold{Examples});}
  \item{\code{cop }}{--- Function of the copula \eqn{\mathbf{C}};}
  \item{\code{cop1}}{--- Alternative naming of the function of the coupla \eqn{\mathbf{C}};}
  \item{\code{para }}{--- Vector of parameters \eqn{\Theta_\mathbf{C}} for  \eqn{\mathbf{C}}; and}
  \item{\code{para1}}{--- Alternative naming of the vector of parameters \eqn{\Theta_\mathbf{C}} for  \eqn{\mathbf{C}}.}
}

The function silently restricts the \eqn{\beta} to its interval as defined, but parameter transform might be useful in some numerical optimization schemes. The following recipes are useful for transform from a parameter in numerical optimization to the asymmetry parameter:
\preformatted{
   #    transform into space for optimization
   BREVEtfunc <- function(p) { return(   qnorm((p[1] + 1) / 2) ) } # [-Inf, +Inf]
   # re-transform back into space for the copula
   BREVErfunc <- function(p) { return(2 * pnorm(p[1]) - 1)       } # [-1  , +1  ]
}
}
\references{
Chang, B., and Joe, H., 2020, Copula diagnostics for asymmetries and conditional
dependence: Journal of Applied Statistics, v. 47, no. 9, pp. 1587--1615, \doi{10.1080/02664763.2019.1685080}.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{convex2COP}}, \code{\link{convexCOP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, \code{\link{composite3COP}}, \code{\link{FRECHETcop}}, \code{\link{glueCOP}}}
\examples{
para <- list(breve=0.24, cop1=FRECHETcop, para1=c(0.4, 0.56))
breveCOP(0.87, 0.35, para=para) # 0.282743

betas <- seq(-1,1, by=0.01)
bloms <- sapply(betas, function(b) {
             breveCOP(0.15, 0.25, para=list(cop=GLPMcop, para=c(2, 2), beta=b))
         } )
plot(betas, bloms, type="l", main="GLPMcop(u,v; 2,2) by breveCOP(beta)")

\dontrun{
  # Notice the argument cop and para name adjustments to show that
  # translation exists inside the function to have use flexibility.
  para <- list(beta=+0.44, cop1=FRECHETcop, para1=c(0.2, 0.56))
  UV   <- simCOP(1000, cop=breveCOP, para=para)
  para <- list(beta=-0.44, cop= FRECHETcop, para= c(0.2, 0.56))
  UV   <- simCOP(1000, cop=breveCOP, para=para) # }

\dontrun{
  # Testing on a comprehensive copula (Plackett)
  betas <- rhos <- thetas <- brhos <- NULL
  for(beta  in seq(-1, 1, by=0.1 )) {
    for(rho in seq(-1, 1, by=0.01))   {
       theta  <- PLACKETTpar(rho=rho, byrho=TRUE)
       thetas <- c(thetas, theta)
       para   <- list(  cop=PLcop,    para=theta, beta=beta)
       brho   <- rhoCOP(cop=breveCOP, para=para)
       betas  <- c(betas, beta); rhos <- c(rhos, rho)
       brhos  <- c(brhos, brho)
    }
  }
  df <- data.frame(beta=betas, theta=thetas, rho=rhos, brho=brhos)
  plot(df$theta, df$brho, log="x", pch=16, cex=0.9, col="seagreen",
       xlab="Plackett parameter", ylab="Spearman Rho")
  lines(df$theta[df$beta == 0], df$brho[df$beta == 0], col="red", lwd=2)
  # Red line is the Plackett in its permutation symmetric definition. #}

\dontrun{
  # Here is an example for a test using mleCOP() to estimate a 5-parameter asymmetric
  # copula model to "some data" on transition from yesterday to today data for a very
  # large daily time series. The purpose of example here is to demonstrate interfacing
  # to the breveCOP() for it to add asymmetry to composition of two copula.
  myASYMCOP <- function(u,v, para, ...) {
    subpara <- list(alpha=para$alpha, beta=para$beta, cop1=GHcop, para1=para$para1,
                                                      cop2=PLcop, para2=para$para2)
    breveCOP(u,v, cop=convex2COP, para=subpara)
  }
  para <- list(alpha=+0.16934027, cop1=GHcop, para1=c(1.11144148, 10.32292673),
                beta=-0.01923808, cop2=PLcop, para2=3721.82966727)
  UV <- simCOP(30000, cop=myASYMCOP, para=para, pch=16, col=grey(0, 0.1))
  abline(0,1, lwd=3, col="red") #}

\dontrun{
  # Here is a demonstration of the permutations of the passing of the
  # asymmetry parameter into the function and then by
  UV <- simCOP(1E3, cop=breveCOP, para=list(cop=HRcop, para=5), breve=+0.5)
  UV <- simCOP(1E3, cop=breveCOP, para=list(cop=HRcop, para=5), breve=-0.5)
  UV <- simCOP(1E3, cop=breveCOP, para=list(cop=HRcop, para=5,  beta =+0.5))
  UV <- simCOP(1E3, cop=breveCOP, para=list(cop=HRcop, para=5,  breve=+0.5))
  UV <- simCOP(1E3, cop=breveCOP, para=list(cop=HRcop, para=5,  beta=-0.4, breve=+0.5))

  para <- list(cop1=HRcop, para1=6, cop2=PSP, para2=NULL, alpha=1, beta=0.7)
  myCOP <- function(u,v, para, ...) breveCOP(u,v, cop=composite2COP, para=para)
  para$breve <- "here I am"
  UV <- simCOP(1E3, cop=composite2COP, para=para, seed=1) # breve is not used
  para$breve <- -0.16
  UV <- simCOP(1E3, cop=myCOP,         para=para, seed=1)
  para$breve <- +0.16
  UV <- simCOP(1E3, cop=myCOP,         para=para, seed=1) # }
}
\keyword{copula composition}
\keyword{copula composition (permutation asymmetric)}
\keyword{permutation asymmetry}
