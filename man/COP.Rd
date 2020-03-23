\encoding{utf8}
\name{COP}
\alias{COP}
\title{ The Copula }
\description{
Compute the \emph{copula} or \emph{joint distribution function} through a copula as shown by Nelsen (2006, p. 18) is the joint probability

\deqn{\mathrm{Pr}[U \le u, V \le v] = \mathbf{C}(u,v)\mbox{.}}

The copula is an expression of the joint probability that both \eqn{U \le u} and \eqn{V \le v}. A \emph{radially symmetric} or \emph{permutation symmetric copula} is one such that \eqn{\mathbf{C}(u,v) = \mathbf{C}(v,u)} otherwise the copula is \emph{asymmetric}. A copula is a type of \emph{dependence function} that permits straightforward characterization of dependence from independence.  Joe (2014, p. 8) comments that \dQuote{copula families are usually given as cdfs [cumulative distribution functions.]}

The copula \emph{inversions} \eqn{t = \mathbf{C}(u{=}U, v)} or \eqn{t = \mathbf{C}(u, v{=}V)} for a given \eqn{t} and \eqn{U} or \eqn{V} are provided by \code{\link{COPinv}} and \code{\link{COPinv2}}, respectively. A copula exists in the domain of the unit square (\eqn{\mathcal{I}^2 = [0, 1]\times [0,1]}) and is a \emph{grounded} function meaning that
\deqn{\mathbf{C}(u,0) = 0 = \mathbf{C}(0,v) \mbox{\ and\ thus\ } \mathbf{C}(0,0) = 0\mbox{, }}
and other properties of a copula are that
\deqn{\mathbf{C}(u,1) = u \mbox{\ and\ } \mathbf{C}(1,v) = v\mbox{\ and}}
\deqn{\mathbf{C}(1,1) = 1\mbox{.}}

Copulas can be combined with each other (\code{\link{convex2COP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, and \cr \code{\link{composite3COP}}) to form more complex and sophisticated dependence structures. Also copula multiplication---a special product of two copulas---yields another copula (see \code{\link{prod2COP}}).

Perhaps the one of the more useful features of this function is that in practical applications it can be used to take a copula formula and reflect or rotated it in fashions to attain association structures that the native definition of the copula can not acquire. The terminal demonstration in the \bold{Examples} demonstrates this for the \emph{Raftery copula} (\code{\link{RFcop}}).
}
\usage{
COP(u, v, cop=NULL, para=NULL,
          reflect=c("cop", "surv", "acute", "grave",
                      "1",    "2",     "3",     "4"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function with vectorization as in \code{asCOP};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula;}
  \item{reflect}{The reflection of the copula form (see \bold{Note}) and the default \code{"cop"} or \code{"1"} is the usual copula definition (also see \code{\link{simCOPmicro}}). The numbered values correspond, respectively, to the named values; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
\emph{REFLECTIONS OF VARIABLES (ROTATIONS OF THE COPULA)}---The copula of \eqn{(1-U, 1-V)} is the survival copula (\eqn{\hat{\mathbf{C}}(u,v)}; \code{\link{surCOP}}) and is defined as
\deqn{\mathrm{Pr}[\,U > u, V > v\,] = \hat{\mathbf{C}}(u,v) = u + v - 1 + \mathbf{C}(1-u,1-v)\:\rightarrow\mbox{\ \code{"surv"},}}
whereas, following the notation of Joe (2014, p. 271), the copula of \eqn{(1-U, V)} is defined as
\deqn{\mathrm{Pr}[\,U > u, V \le v\,] = \acute{\mathbf{C}}(u,v) = v - \mathbf{C}(1-u,v)\:\rightarrow\mbox{\ \code{"acute"}, and}}
 the copula of \eqn{(U, 1-V)} is defined as
\deqn{\mathrm{Pr}[\,U \le u, V > v\,] = \grave{\mathbf{C}}(u,v) = u - \mathbf{C}(u,1-v)\:\rightarrow\mbox{\ \code{"grave"}.}}
Here it is useful to stress the probability aspects that change with the reflections, but this section ends with the reflections themselves being graphically highlighted. The \bold{Examples} stress simple variations on the probability aspects.

To clarify the seemingly clunky nomenclature---Joe (2014) does not provide \dQuote{names} for \eqn{\acute\mathbf{C}(u,v)} or \eqn{\grave\mathbf{C}(u,v)}---the following guidance is informative:\cr
\eqn{\mbox{}\quad\mbox{}}(1) \code{"surv"} or \eqn{\hat\mathbf{C}(u,v)} is a reflection of \eqn{U} and \eqn{V} on the horizontal \emph{and} vertical axes, respectively,\cr
\eqn{\mbox{}\quad\mbox{}}(2) \code{"acute"} or \eqn{\acute\mathbf{C}(u,v)} is a reflection of \eqn{U} on the horizontal axis, and\cr
\eqn{\mbox{}\quad\mbox{}}(3) \code{"grave"} or \eqn{\grave\mathbf{C}(u,v)} is a reflection of \eqn{V} on the verical axis.\cr
The names \code{"acute"} and \code{"grave"} match those used in the \bold{Rd}-format math typesetting instructions. Users are directed to the documentation of \code{\link{simCOPmicro}} for further discussion because the \code{COP} function is expected to be an early entry point for new users interested in the \pkg{copBasic} API.

For the \pkg{copBasic} package and in order to keep some logic brief and code accessible for teaching and applied circumstances, reflections of copulas using analogs to the \code{reflect} argument are only natively supported in the \code{COP} and the \code{\link{simCOPmicro}} functions. The interfaces of \pkg{copBasic} should already be flexible enough for users to adapt and (or) specially name reflections of copulas for deployment.

An example is warranted. Although the Gumbel--Hougaard copula (\code{\link{GHcop}}) can be reflected by \code{COP} and \code{\link{simCOPmicro}} and testing is made in the \bold{Note} section of \code{\link{simCOPmicro}}, it is suggested that a user generally requiring say a horizontal reflection \code{ru} (or vertical reflection \code{rv}) of the Gumbel--Hougaard copula write a function named perhaps \code{ruGHcop} (or \code{rvGHcop}).

Such functions, consistent with the mathematics at the beginning of this \bold{Note}, can be used throughout functions of \pkg{copBasic} using the \code{cop} arguments. The author (Asquith) eschews implementing what is perceived as too much flexibility and overhead for the package to support the three reflection permutations universally across all copula functions of the package. This being said, \code{COP} can take an \R \code{list} for the \code{para} argument for rotation/reflection:
\preformatted{
  set.seed(14)
  UV3 <- simCOP(20, cop=COP, pch=16, col=3,
                para=list(cop=GLcop, para=pi+1, reflect="3"))
  set.seed(14)
  UV2 <- simCOP(20, cop=COP, pch=16, col=4, ploton=FALSE,
                para=list(cop=GLcop, para=pi+1, reflect="2"))
  arrows(x0=UV3[,1], y0=UV3[,2], x=UV2[,1], y=UV2[,2])
}
and this type of interface is similar to \code{\link{composite1COP}} as the following rotation and then asymmetric construction shows:
\preformatted{
  UV <- simCOP(1000, cop=composite1COP,
                     para=list(cop1=COP,
                               para1=c(cop=GHcop, para=pi+1, reflect="4"),
                               alpha=0.1, beta=0.3))
}
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{coCOP}}, \code{\link{duCOP}}, \code{\link{surCOP}}, \code{\link{surfuncCOP}} }
\examples{
u <- runif(1); v <- runif(1)
COP(cop=W,u,v); COP(cop=P,u,v); COP(cop=M,u,v); COP(cop=PSP,u,v)

FF <- 0.75 # 75th percentile, nonexceedance
GG <- 0.20 # 25th percentile, nonexceedance
bF <- 1 - FF; bG <- 1 - GG     # exceedance
# What is the probability that both X and Y are less than
# 75th and 20th percentiles, respectively?
COP(cop=P,FF,GG)    # 0.15
# What is the probability that both X and Y are greater than
# 75th and 20th percentiles, respectively?
surCOP(cop=P,bF,bG) # 0.20
# What is the probability that either X or Y are less than
# the 75th and 20th percentiles, respectively?
duCOP(cop=P,FF,GG)  # 0.8
# What is the probability that either X or Y are greater than
# the 75th and 20th percentiles, respectively?
coCOP(cop=P,bF,bG)  # 0.85

# Repeat for the PSP copula:
# What is the probability that both X and Y are less than
# 75th and 20th percentiles, respectively?
COP(cop=PSP,FF,GG)    # 0.1875
# What is the probability that both X and Y are greater than
# 75th and 20th percentiles, respectively?
surCOP(cop=PSP,bF,bG) # 0.2375
# What is the probability that either X or Y are less than
# the 75th and 20th percentiles, respectively?
duCOP(cop=PSP,FF,GG)  # 0.7625
# What is the probability that either X or Y are greater than
# the 75th and 20th percentiles, respectively?
coCOP(cop=PSP,bF,bG)  # 0.8125
# Both of these summations equal unity
   COP(cop=PSP, FF, GG) + coCOP(cop=PSP, bF, bG) # 1
surCOP(cop=PSP, bF, bG) + duCOP(cop=PSP, FF, GG) # 1

FF <- 0.99 # 99th percentile, nonexceedance
GG <- 0.50 # 50th percentile, nonexceedance
bF <- 1 - FF # nonexceedance
bG <- 1 - GG # nonexceedance
# What is the probability that both X and Y are less than
# 99th and 50th percentiles, respectively?
COP(cop=P,FF,GG)    # 0.495
# What is the probability that both X and Y are greater than
# 99th and 50th percentiles, respectively?
surCOP(cop=P,bF,bG) # 0.005
# What is the probability that either X or Y are less than
# the 99th and 50th percentiles, respectively?
duCOP(cop=P,FF,GG)  # 0.995
# What is the probability that either X or Y are greater than
# the 99th and 50th percentiles, respectively?
coCOP(cop=P,bF,bG)  # 0.505

\dontrun{
# MAJOR EXAMPLE FOR QUICKLY MODIFYING INHERENT ASSOCIATION STRUCTURES
p <- .5 # Reasonable strong positive association for the Raftery copula
"RFcop1" <- function(u,v, para) COP(u,v, cop=RFcop, para=p, reflect="1")
"RFcop2" <- function(u,v, para) COP(u,v, cop=RFcop, para=p, reflect="2")
"RFcop3" <- function(u,v, para) COP(u,v, cop=RFcop, para=p, reflect="3")
"RFcop4" <- function(u,v, para) COP(u,v, cop=RFcop, para=p, reflect="4")

d <- 0.01 # Just to speed up the density plots a bit
densityCOPplot(RFcop1, para=p, contour.col=1, deluv=d) # the Raftery in the literature
densityCOPplot(RFcop2, para=p, contour.col=1, deluv=d, ploton=FALSE)
densityCOPplot(RFcop3, para=p, contour.col=1, deluv=d, ploton=FALSE)
densityCOPplot(RFcop4, para=p, contour.col=1, deluv=d, ploton=FALSE)
# Now some text into the converging tail to show the reflection used.
text(-2,-2, "reflect=1", col=2); text(+2,+2, "reflect=2", col=2)
text(+2,-2, "reflect=3", col=2); text(-2,+2, "reflect=4", col=2) #}
}
\keyword{copula (formulas)}
\keyword{copula operator}
\keyword{joint probability}
\keyword{copula (conditional distribution)}
\keyword{copula (conditional quantile function)}
\keyword{copula (reflection)}

