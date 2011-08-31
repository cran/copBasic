\name{sectionCOP}
\alias{sectionCOP}
\title{ The Sections or Derivative of Sections of a Copula }
\description{
Compute
}
\usage{
sectionCOP(f, cop=NULL, para=NULL, wrtV=FALSE, ratcop=FALSE,
           dercop=FALSE, ploton=TRUE, lines=TRUE, delt=0.005, ...)
}
\arguments{
  \item{f}{The value of probability \eqn{u} or \eqn{v} along the horizontal or vertical axis of the unit square;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{wrtV}{A logical to toggle between with respect to \eqn{v} or \eqn{u} (default);}
  \item{ratcop}{Compute the ratio \eqn{\mathbf{C}(u,v)/(u \mbox{or} v)};}
  \item{dercop}{Compute the derivative along the section;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{lines}{Draw the lines of diagonal to the current device;}
  \item{delt}{The increment of the level curves to plot, defaults to 5-percent intervals; and}
  \item{...}{Additional arguments to pass to the \code{plot()} and \code{lines()} functions.}
}
\value{
  A \code{list} of the .
}
\note{
See pp. 13--14 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{diagCOP}}}
\examples{


tmp <- sectionCOP(.55,cop=PSP, lwd=5, col=2)
tmp <- sectionCOP(.55,cop=PSP, wrtV=TRUE, ploton=FALSE,
                               lwd=2, col=3)
tmp <- sectionCOP(.85,cop=PSP, ploton=FALSE, lwd=5, col=2, lty=2)
tmp <- sectionCOP(.85,cop=PSP, wrtV=TRUE, ploton=FALSE,
                               lwd=2, col=3, lty=2)


tmp <- sectionCOP(.35,cop=PSP, col=3, lwd=5)
tmp <- sectionCOP(.35,cop=PSP, dercop=TRUE,
                               ploton=FALSE, col=3)

tmp <- sectionCOP(.35,cop=PSP, wrtV=TRUE, col=2, lwd=5)
tmp <- sectionCOP(.35,cop=PSP, wrtV=TRUE, dercop=TRUE,
                               ploton=FALSE, col=2)

\dontrun{
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=10^runif(1,min=-4, max=0),
             para2=10^runif(1,min= 0, max=4),
             alpha=runif(1), beta=runif(1))
txts <- c("Alpha=",    round(para$alpha, digits=4),
          "; Beta=",   round(para$beta,  digits=4),
          "; Theta1=", round(para$para1[1], digits=5),
          "; Theta2=", round(para$para2[1], digits=2))
layout(matrix(1:2,byrow=TRUE))
D <- simCOP(n=300, cop=composite2COP, para=para,
            cex=0.5, col=rgb(0,0,0,0.2), pch=16)
mtext(paste(txts,collapse=""))

f <- round(runif(1),digits=2)
segments(f,0,f,1, col=3, lwd=2); segments(0,f,1,f, col=2, lwd=2)
ftxt <- c("Sectionals (thick) and derivatives (thin) at ",f,
          " nonexceedance probability")
tmp <- sectionCOP(f, cop=composite2COP, para=para, col=3, lwd=4)
tmp <- sectionCOP(f, cop=composite2COP, para=para, dercop=TRUE,
                     ploton=FALSE, col=3)
tmp <- sectionCOP(f, cop=composite2COP, para=para, ratcop=TRUE,
                     ploton=FALSE, col=3, lty=2)

tmp <- sectionCOP(f, cop=composite2COP, para=para, wrtV=TRUE,
                     ploton=FALSE, col=2, lwd=4)
tmp <- sectionCOP(f, cop=composite2COP, para=para, wrtV=TRUE,
                     dercop=TRUE, ploton=FALSE, col=2)
tmp <- sectionCOP(f, cop=composite2COP, para=para, wrtV=TRUE,
                     ratcop=TRUE, ploton=FALSE, col=2)
mtext(paste(ftxt, collapse=""))
}
}
\keyword{distribution}
\keyword{multivariate}
