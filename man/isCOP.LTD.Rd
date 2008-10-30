\name{isCOP.LTD}
\alias{isCOP.LTD}
\title{ Is a Copula Left-Tail Decreasing }
\description{
Numerically set a logical whether a copula is left-tail decreasing. A copula \eqn{\mathbf{C}(u,v)} is left-tail decreasing for \eqn{LTD(X \mid Y)} or \eqn{LTD(V \mid U)} if, and only if, for every \eqn{v \in [0,1]},
\deqn{\frac{\delta \mathbf{C}(u,v)}{\delta u} \le \frac{\mathbf{C}(u,v)}{u}\mbox{.}}

The \eqn{LTD} test is associated with the concept of \dQuote{tail monotonicity}. \eqn{LTD(V \mid U)} means that the random variable \eqn{Y} is more likely to acquire smaller values as random variable \eqn{X} decreases. Finally, the accuracy of the numerical determination of the logical is dependent on the the smallness of the \code{delt} argument passed into the function.

The opposite with-respect-to \eqn{LTD(X \mid Y)} is left-tail decreasing (\eqn{LTD(U \mid V)}) if, and only if, for every \eqn{u \in [0,1]},
\deqn{\frac{\delta \mathbf{C}(u,v)}{\delta v} \le \frac{\mathbf{C}(u,v)}{v}\mbox{,}}
which is controlled by the \code{wrtV=TRUE} argument.
}
\usage{
isCOP.LTD(cop=NULL, para=NULL, wrtV=FALSE, verbose=FALSE,
          delt=0.005, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{wrtV}{A logical to toggle between with respect to \eqn{v} or \eqn{u} (default);}
  \item{verbose}{A logical to show the looping index on the outer loop;}
  \item{delt}{The increment of axis set by \code{wrtV}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A logical \code{TRUE} or \code{FALSE} is returned.
}
\note{
See p.~222 of the reference.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in nature---An approach using copulas: Dordrecht, Netherlands, Springer, Water Science and Technology Library 56, 292~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{isCOP.RTI}}}
\examples{
\dontrun{
isCOP.LTD(cop=P, delt=.01) # independency should be FALSE
# FALSE

# Positive association
isCOP.LTD(cop=PSP) # compute fast, has lower-tail dependency
# TRUE

# Negative association Plackett
isCOP.LTD(cop=PLACKETTcop, para=c(.15))
# FALSE

# Positive association Plackett
isCOP.LTD(cop=PLACKETTcop, para=c(15))
# TRUE



# Negative association Plackett
isCOP.LTD(cop=PLACKETTcop, wrtv=TRUE, para=c(.15))
# FALSE

# Positive association Plackett
isCOP.LTD(cop=PLACKETTcop, wrtV=TRUE, para=c(15))
# TRUE
}
}
\keyword{distribution}
\keyword{multivariate}
