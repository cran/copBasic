\encoding{utf8}
\name{simCOPmicro}
\alias{simCOPmicro}
\alias{simCOPv}
\title{ Simulate V from U through a Copula by Numerical Derivative Method }
\description{
Perform bivariate simulation of random but coupled variables \eqn{V} from \eqn{U} through a copula (Nelsen, 2006, p. 32) by inversion of the numerical derivatives of the copula (\code{\link{derCOPinv}}, \code{\link{derCOPinv2}}). The method is more broadly known as \emph{conditional simulation method}. An elaborate implementation is available in \code{\link{simCOP}}, which unlike \code{simCOPmicro}, has provisions (default) for graphical support. The \code{simCOPmicro} function is intended to be a minimalist version for copula simulation, and such a version is useful for pedagogic purposes including \emph{conditional distributions}, \emph{conditional quantile functions}, and \emph{copula reflection} (see \bold{Note} and \code{\link{COP}}). An extended educational discussion of simulation using the conditional method is available in the \bold{Note} section of \code{\link{derCOPinv}}.

Some definitions are needed. The copula of \eqn{(1-U, 1-V)} is the \emph{survival copula} (\code{\link{surCOP}}) and is defined as
\deqn{\hat{\mathbf{C}}(u,v) = u + v - 1 + \mathbf{C}(1-u,1-v)\mbox{,}}
whereas, following the notation of Joe (2014, pp. 271--272), the copula of \eqn{(1-U, V)} is defined as
\deqn{\acute{\mathbf{C}}(u,v) = v - \mathbf{C}(1-u,v)\mbox{, and}}
 the copula of \eqn{(U, 1-V)} is defined as
\deqn{\grave{\mathbf{C}}(u,v) = u - \mathbf{C}(u,1-v)\mbox{.}}

Careful consideration of the nomenclature is necessary as confusion with the occurrences of \eqn{1-u} and \eqn{1-v} easily conflate meaning. The nomenclature for the \emph{survival copula} is more elaborately shown under \code{\link{surCOP}}. The difficulty is that the bivariate arguments to the \emph{survival copula} are \emph{exceedance probabilities}.

For simulation, again following the nomenclature of Joe (2014, p. 272), the conditional distribution functions (numerical derivatives; \code{\link{derCOP}} \eqn{\equiv} \eqn{\mathbf{C}_{2|1}(v|u)} and \code{\link{derCOP2}} \eqn{\equiv} \eqn{\mathbf{C}_{1|2}(u|v)}) can be written in terms of \eqn{\mathbf{C}(u|v) = \mathbf{C}_{2|1}(v|u)} as
\deqn{  \hat\mathbf{C}_{2|1}(v|u) = 1 - \mathbf{C}_{2|1}(1-v|1-u)\mbox{,}}
\deqn{\acute\mathbf{C}_{2|1}(v|u) =     \mathbf{C}_{2|1}(  v|1-u)\mbox{, and}}
\deqn{\grave\mathbf{C}_{2|1}(v|u) = 1 - \mathbf{C}_{2|1}(1-v|  u)\mbox{,}}
where the respective \code{"surv"}, \code{"acute"}, and \code{"grave"} are inverses (conditional quantile functions; inverses of numerical derivatives; \code{\link{derCOPinv}} \eqn{\equiv} \eqn{\mathbf{C}^{(-1)}_{2|1}(v|u)} and \code{\link{derCOPinv2}} \eqn{\equiv} \eqn{\mathbf{C}^{(-1)}_{1|2}(u|v)}) are
\deqn{  \hat\mathbf{C}^{(-1)}_{2|1}(t|u) = 1 - \mathbf{C}^{(-1)}_{2|1}(1-t|1-u)\:\rightarrow\mbox{\ \code{"sur"},}}
\deqn{\acute\mathbf{C}^{(-1)}_{2|1}(t|u) =     \mathbf{C}^{(-1)}_{2|1}(  t|1-u)\:\rightarrow\mbox{\ \code{"acute"}, and}}
\deqn{\grave\mathbf{C}^{(-1)}_{2|1}(t|u) = 1 - \mathbf{C}^{(-1)}_{2|1}(1-t|  u)\:\rightarrow\mbox{\ \code{"grave"},}}
where \eqn{t} is a uniformly distributed variable.

To clarify the seemingly clunky nomenclature---Joe (2014) does not provide \dQuote{names} for \eqn{\acute\mathbf{C}(u,v)} or \eqn{\grave\mathbf{C}(u,v)}---the following guidance is informative:\cr
\eqn{\mbox{}\quad\mbox{}}(1) \code{"surv"} or \eqn{\hat\mathbf{C}(u,v)} is a reflection of \eqn{U} and \eqn{V} on the horizontal \emph{and} vertical axes, respectively\cr
\eqn{\mbox{}\quad\mbox{}}(2) \code{"acute"} or \eqn{\acute\mathbf{C}(u,v)} is a reflection of \eqn{U} on the horizontal axis, and\cr
\eqn{\mbox{}\quad\mbox{}}(3) \code{"grave"} or \eqn{\grave\mathbf{C}(u,v)} is a reflection of \eqn{V} on the verical axis.\cr
The names \code{"acute"} and \code{"grave"} match those used in the \bold{Rd}-format math typesetting instructions.
}
\usage{
simCOPmicro(u, cop=NULL, para=NULL,
               reflect=c("cop", "surv", "acute", "grave",
                           "1",    "2",     "3",     "4"), ...)
simCOPv(u, cop=NULL, para=NULL,
               reflect=c("cop", "surv", "acute", "grave",
                           "1",    "2",     "3",     "4"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction. The \code{runif()} function in \R can be used to drive conditional simulation using the \code{simCOPmicro} function (see \bold{Examples});}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{reflect}{The reflection of the copula (see above) and the default \code{"cop"} or \code{"1"} is the usual copula definition.  The numbered values correspond, respectively, to the named values; and}
  \item{...}{Additional arguments to pass should they be needed.}
}
\value{
  Simulated value(s) of nonexceedance probability \eqn{v} are returned based on the nonexceedance probabilities \eqn{u} in argument \code{u}.
}
\note{
The advanced features of \code{simCOPmicro} permit simulation of the three permutations of \emph{variable reflection}. The first code simply produce four different \dQuote{copulas} based on the \emph{Gumbel--Hougaard} copula (\eqn{\mathbf{GH}(u,v; \Theta)}; \code{\link{GHcop}}), which has substantial upper tail dependency but no lower tail dependency for \eqn{\Theta = 2.512} as quantified by the \code{\link{taildepCOP}} function call.
\preformatted{
  U <- runif(1500); G <- 2.512; u <- 0.1; up <- 1-u; v <- 0.2; vp <- 1-v
  UV   <- data.frame(U, simCOPmicro(U, cop=GHcop, para=G              ))
  sUsV <- data.frame(U, simCOPmicro(U, cop=GHcop, para=G, reflect="surv" ))
  sUV  <- data.frame(U, simCOPmicro(U, cop=GHcop, para=G, reflect="acute"))
  UsV  <- data.frame(U, simCOPmicro(U, cop=GHcop, para=G, reflect="grave"))
  taildepCOP(cop=GHcop, para=G) # lambdaL = 2e-05; lambdaU = 0.68224
}
The following code example will verify that the simulations produce values of \eqn{U} and \eqn{V} that are consistent with the \emph{empirical copula} (\code{\link{EMPIRcop}}) results as well as consistent with the variable reflections provided through the \code{\link{COP}} interface. Notice the combinations of nonexceedance and exceedance probabilities blended so that the two returned values for the four different copulas are numerically congruent.
\preformatted{
  c(EMPIRcop(u, v,  para=UV),   COP(u, v,  cop=GHcop, para=G, reflect="cop"  ))
  c(EMPIRcop(up,vp, para=sUsV), COP(up,vp, cop=GHcop, para=G, reflect="surv" ))
  c(EMPIRcop(up,v,  para=sUV),  COP(up,v,  cop=GHcop, para=G, reflect="acute"))
  c(EMPIRcop(u, vp, para=UsV),  COP(u, vp, cop=GHcop, para=G, reflect="grave"))
}
The user can verify the reflections graphically using code such as this
\preformatted{
  xlab <- "PROBABILITY IN U"; ylab <- "PROBABILITY IN V"
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE));
  plot(UV,   xlab=xlab, ylab=ylab, pch=3, lwd=0.5, col=1)
    mtext("no reflection")
  plot(sUV,  xlab=xlab, ylab=ylab, pch=3, lwd=0.5, col=3)
    mtext("horizontal reflection")
  plot(UsV,  xlab=xlab, ylab=ylab, pch=3, lwd=0.5, col=4)
    mtext("vertical reflection")
  plot(sUsV, xlab=xlab, ylab=ylab, pch=3, lwd=0.5, col=2)
    mtext("double reflection")
}
in which inspection of tails exhibiting the dependency is readily seen on the four plots: upper right tail dependency (no reflection), upper left tail dependency (horizontal reflection), lower right tail dependency (vertical reflection), and lower left tail dependency (double reflection). It is important to stress that these descriptions and graphical depictions of single tail dependency are specific to the \eqn{\mathbf{GH}(u,v; \Theta)} copula chosen for the demonstration.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{simCOP}}}
\examples{
simCOPmicro(runif(1), cop=W  ) # Frechet lower bound copula
simCOPmicro(runif(1), cop=P  ) # Independence copula
simCOPmicro(runif(1), cop=M  ) # Frechet upper bound copula
simCOPmicro(runif(1), cop=PSP) # The PSP copula

# Now let us get more complicated and mix two Plackett copulas together using the
# composite2COP as a "compositor." The parameter argument becomes more complex, but is
# passed as shown into composite2COP.
para <- list(cop1=PLACKETTcop,cop2=PLACKETTcop, alpha=0.3,beta=0.5, para1=0.1,para2=50)
simCOPmicro(runif(5), cop=composite2COP, para=para)

\dontrun{
# Now let us implement "our" own version of features of simCOP() but using
# the micro version to manually create just the simulation vector of V.
U <- runif(1500)
UV <- data.frame(U, simCOPmicro(U, cop=N4212cop, para=4))
plot(UV, xlab="PROBABILITY IN U", ylab="PROBABILITY IN V", pch=3, col=2) #}
}
\keyword{copula (simulation)}
\keyword{copula (conditional distribution)}
\keyword{copula (conditional quantile function)}
\keyword{copula (reflection)}
