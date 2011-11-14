\name{composite3COP}
\alias{composite3COP}
\title{ Extended Composition of Two Copulas }
\description{
The construction of an asymmetric copula from composition two copulas provide for more sophisticated structures of dependence between variables. Let \eqn{\mathbf{A}} and \eqn{\mathbf{B}} be copulas with their own parameter sets. Then

\deqn{\mathbf{C}_{\alpha,\beta,\kappa,\gamma}(u,v) = u^\kappa v^\gamma
                              \mathbf{A}([u^{1-\kappa}]^\alpha,    [v^{1-\gamma}]^\beta) \cdot
                              \mathbf{B}([u^{1-\kappa}]^{1-\alpha},[v^{1-\gamma}]^{1-\beta})\mbox{,}}

defines a family of copulas \eqn{\mathbf{C}_{\alpha,\beta,\kappa,\gamma}}, with parameters \eqn{\alpha,\beta,\kappa,\gamma \in \mathcal{I}:[0,1]}.
}
\usage{
composite3COP(u,v,para,...)
}
\arguments{
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction,}
  \item{para}{A special parameter \code{list()} (see Note), and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A value for the composited copula is returned.
}
\note{
See p. 266 of the reference and Proposition C.2 for complete discussion.

The following descriptions list in detail the structure and content of the \code{para} argument:
\describe{
  \item{\samp{alpha}}{The \eqn{\alpha} parameter,}
  \item{\samp{beta}}{The \eqn{\beta} parameter,}
  \item{\samp{kappa}}{The \eqn{\kappa} parameter,}
  \item{\samp{gamma}}{The \eqn{\gamma} parameter,}
  \item{\samp{cop1}}{Function of the first copula,}
  \item{\samp{cop2}}{Function of the second copula,}
  \item{\samp{para1}}{Vector of parameters for the first copula, and}
  \item{\samp{para2}}{Vector of parameters for the second copula.}
}
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007,
Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{simCOP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, \code{\link{simcomposite3COP}}}
\examples{
\dontrun{
para <- list(cop1=PLACKETTcop,
             cop2=N4.2.12cop,
             para1=10^(runif(1,min=-5,max=5)),
             para2=runif(1,min=1,max=100),
             alpha=runif(1), beta=runif(1),
             kappa=runif(1), gamma=runif(1))
txts <- c("Alpha=",    round(para$alpha, digits=4),
          "; Beta=",   round(para$beta,  digits=4),
          "; Kappa=",    round(para$kappa, digits=4),
          "; Gamma=",   round(para$gamma,  digits=4),
          "; Theta1=", round(para$para1[1], digits=5),
          "; Theta2=", round(para$para2[1], digits=2))
layout(matrix(1:2,byrow=TRUE))
D <- simCOP(n=300, cop=composite3COP, para=para,
            cex=0.5, col=rgb(0,0,0,0.2), pch=16)
mtext(paste(txts,collapse=""))

f <- round(runif(1),digits=2)
ftxt <- c("Sectionals (thick) and derivatives (thin) at ", f,
          " nonexceedance probability")
segments(f,0,f,1, col=3, lwd=2); segments(0,f,1,f, col=2, lwd=2)
tmp <- sectionCOP(f, cop=composite3COP, para=para, col=3, lwd=4)
tmp <- sectionCOP(f, cop=composite3COP, para=para, dercop=TRUE,
                     ploton=FALSE, col=3)
tmp <- sectionCOP(f, cop=composite3COP, para=para, wrtV=TRUE,
                     ploton=FALSE, col=2, lwd=4)
tmp <- sectionCOP(f, cop=composite3COP, para=para, wrtV=TRUE,
                     dercop=TRUE, ploton=FALSE, col=2)
mtext(paste(ftxt, collapse=""))
}

\dontrun{
mainpara <- list(cop1=PLACKETTcop,
                 cop2=PLACKETTcop,
                 para1gen=function() {
                    return(10^runif(1, min=-5, max=5)) },
                 para2gen=function() {
                    return(10^runif(1, min=-5, max=5)) })

n.sample <- 200
n <- 20000 # really used 20000 for generation of the data set
PlackettPlackettABKG <-
    simcomposite3COP(n=n.sample, nsim=n, parent=mainpara)
save(PlackettPlackettABKG,
     file="PlackettPlackettABKG.RData", compress="xz")
}
}
\keyword{distribution}
\keyword{multivariate}
