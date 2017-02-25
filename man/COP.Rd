\name{COP}
\alias{COP}
\title{ The Copula }
\description{
Compute the \emph{copula} or \emph{joint distribution function} through a copula as shown by Nelsen (2006, p. 18) is the joint probability

\deqn{\mathrm{Pr}[U \le u, V \le v] = \mathbf{C}(u,v)\mbox{.}}

The copula is an expression of the joint probability that both \eqn{U \le u} and \eqn{V \le v}. A \emph{radial} or \emph{permutation symmetric copula} is one such that \eqn{\mathbf{C}(u,v) = \mathbf{C}(v,u)} otherwise the copula is \emph{asymmetric}. A copula is a type of \emph{dependence function} that permits straightforward characterization of dependence from independence.  Joe (2014, p. 8) comments that \dQuote{copula families are usually given as cdfs [cumluative distribution functions.]}

The copula \emph{inversions} \eqn{t = \mathbf{C}(u{=}U, v)} or \eqn{t = \mathbf{C}(u, v{=}V)} for a given \eqn{t} and \eqn{U} or \eqn{V} are provided by \code{\link{COPinv}} and \code{\link{COPinv2}}, respectively. A copula exists in the domain of the unit square (\eqn{\mathcal{I}^2 = [0, 1]\times [0,1]}) and is a \emph{grounded} function meaning that
\deqn{\mathbf{C}(u,0) = 0 = \mathbf{C}(0,v) \mbox{\ and\ thus\ } \mathbf{C}(0,0) = 0\mbox{, }}
and other properties of a copula are that
\deqn{\mathbf{C}(u,1) = u \mbox{\ and\ } \mathbf{C}(1,v) = v\mbox{\ and}}
\deqn{\mathbf{C}(1,1) = 1\mbox{.}}

Copulas can be combined with each other (\code{\link{convex2COP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, and \cr \code{\link{composite3COP}}) to form more complex and sophisticated dependence structures. Also copula multiplication---a special product of two copulas---yields another copula (see \code{\link{prod2COP}}).
}
\usage{
COP(u, v, cop=NULL, para=NULL, reflect=c("cop", "surv", "acute", "grave"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function with vectorization as in \code{asCOP};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula;}
  \item{reflect}{The reflection of the copula form (see \bold{Note}) and the default \code{"cop"} is the usual copula definition (also see \code{\link{simCOPmicro}}); and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
\emph{REFLECTIONS OF VARIABLES}---The copula of \eqn{(1-U, 1-V)} is the survival copula (\eqn{\hat{\mathbf{C}}(u,v)}; \code{\link{surCOP}}) and is defined as
\deqn{\mathrm{Pr}[\,U > u, V > v\,] = \hat{\mathbf{C}}(u,v) = u + v - 1 + \mathbf{C}(1-u,1-v)\:\rightarrow\mbox{\ \code{"surv"},}}
whereas, following the notation of Joe (2014, p. 271), the copula of \eqn{(1-U, V)} is defined as
\deqn{\mathrm{Pr}[\,U > u, V \le v\,] = \acute{\mathbf{C}}(u,v) = v - \mathbf{C}(1-u,v)\:\rightarrow\mbox{\ \code{"acute"}, and}}
 the copula of \eqn{(U, 1-V)} is defined as
\deqn{\mathrm{Pr}[\,U \le u, V > v\,] = \grave{\mathbf{C}}(u,v) = u - \mathbf{C}(u,1-v)\:\rightarrow\mbox{\ \code{"grave"}.}}

To clarify the seemingly clunky nomenclature---Joe (2014) does not provide \dQuote{names} for \eqn{\acute\mathbf{C}(u,v)} or \eqn{\grave\mathbf{C}(u,v)}---the following guidance is informative:\cr
\eqn{\mbox{}\quad\mbox{}}(1) \code{"surv"} or \eqn{\hat\mathbf{C}(u,v)} is a reflection of \eqn{U} and \eqn{V} on the horizontal \emph{and} vertical axes, respectively,\cr
\eqn{\mbox{}\quad\mbox{}}(2) \code{"acute"} or \eqn{\acute\mathbf{C}(u,v)} is a reflection of \eqn{U} on the horizontal axis, and\cr
\eqn{\mbox{}\quad\mbox{}}(3) \code{"grave"} or \eqn{\grave\mathbf{C}(u,v)} is a reflection of \eqn{V} on the verical axis.\cr
The names \code{"acute"} and \code{"grave"} match those used in the \bold{Rd}-format math typesetting instructions. Users are directed to the documentation of \code{\link{simCOPmicro}} for further discussion because the \code{COP} function is expected to be an early entry point for new users interested in the \pkg{copBasic} API.

For the \pkg{copBasic} package in order to keep some logic brief and code accessible for teaching and applied circumstances, reflections of copulas using analogs to the \code{reflect} argument are only natively supported in the \code{COP} and the \code{\link{simCOPmicro}} functions. The interfaces of \pkg{copBasic} should already be flexible enough for users to adapt and (or) specially name reflections of copulas for deployment.

An example is warranted. Although the Gumbel-Hougaard copula (\code{\link{GHcop}}) can be reflected by \code{COP} and \code{\link{simCOPmicro}} and testing can be made such as in the \bold{Note} section of \code{\link{simCOPmicro}}, it is suggested that a user requiring say a horizontal reflection \code{ru} (or vertical reflection \code{rv}) of the Gumbel-Hougaard copula write a function named perhaps \code{ruGHcop} (or \code{rvGHcop}). Such functions, if properly formed, can be used throughout functions of \pkg{copBasic} using the \code{cop} arguments. The author (Asquith) eschews implementing what is perceived as too much flexibility and overhead for the package to support the three reflection permutations universally across all applicable functions of \pkg{copBasic}.
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

F <- 0.75 # 75th percentile, nonexceedance
G <- 0.20 # 25th percentile, nonexceedance
bF <- 1 - F; bG <- 1 - G      # exceedance

# What is the probability that both X and Y are less than
# 75th and 20th percentiles, respectively?
COP(cop=P,F,G)

# What is the probability that both X and Y are greater than
# 75th and 20th percentiles, respectively?
surCOP(cop=P,bF,bG)

# What is the probability that either X or Y are less than
# the 75th and 20th percentiles, respectively?
duCOP(cop=P,F,G)

# What is the probability that either X or Y are greater than
# the 75th and 20th percentiles, respectively?
coCOP(cop=P,bF,bG)

# Repeat for the PSP copula:
# What is the probability that both X and Y are less than
# 75th and 20th percentiles, respectively?
COP(cop=PSP,F,G)

# What is the probability that both X and Y are greater than
# 75th and 20th percentiles, respectively?
surCOP(cop=PSP,bF,bG)

# What is the probability that either X or Y are less than
# the 75th and 20th percentiles, respectively?
duCOP(cop=PSP,F,G)

# What is the probability that either X or Y are greater than
# the 75th and 20th percentiles, respectively?
coCOP(cop=PSP,bF,bG)

# Both of these summations equal unity
   COP(cop=PSP,  F,  G) + coCOP(cop=PSP, bF, bG)
surCOP(cop=PSP, bF, bG) + duCOP(cop=PSP,  F,  G)

F <- 0.99 # 99th percentile, nonexceedance
G <- 0.50 # 50th percentile, nonexceedance
bF <- 1 - F # nonexceedance
bG <- 1 - G # nonexceedance

# What is the probability that both X and Y are less than
# 99th and 50th percentiles, respectively?
COP(cop=P,F,G)

# What is the probability that both X and Y are greater than
# 99th and 50th percentiles, respectively?
surCOP(cop=P,bF,bG)

# What is the probability that either X or Y are less than
# the 99th and 50th percentiles, respectively?
duCOP(cop=P,F,G)

# What is the probability that either X or Y are greater than
# the 99th and 50th percentiles, respectively?
coCOP(cop=P,bF,bG)
}
\keyword{copula (formulas)}
\keyword{copula operator}
\keyword{joint probability}
\keyword{copula (conditional distribution)}
\keyword{copula (conditional quantile function)}
\keyword{copula (reflection)}

