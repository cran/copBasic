\encoding{utf8}
\name{composite3COP}
\alias{composite3COP}
\title{ (Extended) Composition of Two Copulas with Four Compositing Parameters }
\description{
The \emph{(extended) composition of two copulas}  (Salvadori \emph{et al.}, 2006, p. 266, prop. C.4) provides for even more sophisticated structures of dependence between variables than two-copula composition in \code{\link{composite2COP}}. Let \eqn{\mathbf{A}} and \eqn{\mathbf{B}} be copulas with respective parameters \eqn{\Theta_\mathbf{A}} and \eqn{\Theta_\mathbf{B}}, then

\deqn{\mathbf{C}_{\alpha,\beta,\kappa,\gamma}(u,v) = u^\kappa v^\gamma \cdot
                              \mathbf{A}([u^{1-\kappa}]^\alpha,    [v^{1-\gamma}]^\beta) \cdot
                              \mathbf{B}([u^{1-\kappa}]^{1-\alpha},[v^{1-\gamma}]^{1-\beta})\mbox{,}}

defines a family of copulas \eqn{\mathbf{C}_{\alpha,\beta,\kappa,\gamma}} with \bold{four} \emph{compositing parameters} \eqn{\alpha,\beta,\kappa,\gamma \in \mathcal{I}:[0,1]}.

It is important to stress that copulas \eqn{\mathbf{A}_{\Theta_A}} and \eqn{\mathbf{B}_{\Theta_B}} can be of different families and each parameterized accordingly by the vectors of parameters \eqn{\Theta_A} and \eqn{\Theta_B}. This is an interesting observation in the context of building complex copulatic structures in pursuit of fitting asymmetric measures of dependency such as the \emph{L-comoments} available in the \pkg{lmomco} package.
}
\usage{
composite3COP(u,v,para,...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in \eqn{Y} direction;}
  \item{para}{A special parameter \code{list} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass to \code{\link{composite2COP}}.}
}
\value{
  A value for the composited copula is returned.
}
\note{
The following descriptions list in detail the structure and content of the \code{para} argument:
\describe{
  \item{\code{alpha}}{--- The \eqn{\alpha} compositing parameter;}
  \item{\code{beta}}{--- The \eqn{\beta} compositing parameter;}
  \item{\code{kappa}}{--- The \eqn{\kappa} compositing parameter;}
  \item{\code{gamma}}{--- The \eqn{\gamma} compositing parameter;}
  \item{\code{cop1}}{--- Function of the first copula \eqn{\mathbf{A}};}
  \item{\code{cop2}}{--- Function of the second copula \eqn{\mathbf{B}};}
  \item{\code{para1}}{--- Vector of parameters \eqn{\Theta_\mathbf{A}} for \eqn{\mathbf{A}}; and}
  \item{\code{para2}}{--- Vector of parameters \eqn{\Theta_\mathbf{B}} for \eqn{\mathbf{B}}.}

The first example produces two plots. These are extremely informative for many nuances of copula theory. Whereas it is difficult in prose to describe, users are strongly encouraged that once full understanding of connection of red and green between the easier to understand bivariate plot and the plot showing the sections and derivatives of the sections is achieved that much of copula theory will be mastered---get a copy of Nelsen (2006) and (or) Salvadori \emph{et al.} (2007).
}
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{simCOP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, \code{\link{simcomposite3COP}}}
\examples{
\dontrun{
para <- list(cop1=PLACKETTcop, cop2=N4212cop,
             para1=10^(runif(1,min=-5,max=5)), para2=runif(1,min=1,max=100),
             alpha=runif(1), beta=runif(1), kappa=runif(1), gamma=runif(1))
txts <- c("Alpha=",    round(para$alpha,    digits=4),
          "; Beta=",   round(para$beta,     digits=4),
          "; Kappa=",  round(para$kappa,    digits=4),
          "; Gamma=",  round(para$gamma,    digits=4),
          "; Theta1=", round(para$para1[1], digits=5),
          "; Theta2=", round(para$para2[1], digits=2))
layout(matrix(1:2,byrow=TRUE))
D <- simCOP(n=300, cop=composite3COP, para=para, cex=0.5, col=rgb(0,0,0,0.2), pch=16)
mtext(paste(txts,collapse=""))

f <- round(runif(1),digits=2)
ftxt <- c("Sectionals (thick) and derivatives (thin) at f=",f," nonexceedance prob.")
segments(f,0,f,1, col=3, lwd=2); segments(0,f,1,f, col=2, lwd=2)
t <- sectionCOP(f,cop=composite3COP,para=para, col=3, lwd=4)
t <- sectionCOP(f,cop=composite3COP,para=para, dercop=TRUE, ploton=FALSE,col=3)
t <- sectionCOP(f,cop=composite3COP,para=para, wrtV=TRUE,   ploton=FALSE,col=2,lwd=4)
t <- sectionCOP(f,cop=composite3COP,para=para, wrtV=TRUE,   ploton=FALSE,col=2,
                  dercop=TRUE)
mtext(paste(ftxt, collapse=""))#}
}
\keyword{copula composition}
\keyword{copula composition (four compositing parameters)}

