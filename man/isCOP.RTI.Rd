\name{isCOP.RTI}
\alias{isCOP.RTI}
\title{ Is a Copula Right-Tail Increasing }
\description{
Numerically set a logical whether a copula is right-tail increasing. A copula \eqn{\mathbf{C}(u,v)} is right-tail decreasing for \eqn{RTI(X \mid Y)} or \eqn{RTI(V \mid U)} if, and only if, for every \eqn{v \in [0,1]},
\deqn{\frac{\delta \mathbf{C}(u,v)}{\delta u} \le \frac{v - \mathbf{C}(u,v)}{1 - u}\mbox{.}}

The \eqn{RTI} test is associated with the concept of \dQuote{tail monotonicity}. \eqn{RTI(V \mid U)} means that the random variable \eqn{Y} is more likely to acquire larger values as random variable \eqn{X} increases. Finally, the accuracy of the numerical determination of the logical is dependent on the the smallness of the \code{delt} argument passed into the function.

The opposite with-respect-to \eqn{RTI(X \mid Y)} is right-tail decreasing (\eqn{RTI(U \mid V)}) if, and only if, for every \eqn{u \in [0,1]},
\deqn{\frac{\delta \mathbf{C}(u,v)}{\delta v} \le \frac{u - \mathbf{C}(u,v)}{1- v}\mbox{,}}
which is controlled by the \code{wrtV=TRUE} argument.
}
\usage{
isCOP.RTI(cop=NULL, para=NULL, wrtV=FALSE, verbose=FALSE,
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
See p. 222 of the reference.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in nature---An approach using copulas: Dordrecht, Netherlands, Springer, Water Science and Technology Library 56, 292 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{isCOP.LTD}}}
\examples{
\dontrun{
isCOP.RTI(cop=P, delt=.01) # independency should be FALSE
# but function returns TRUE, note that same logic for isCOP.LTD returns
# FALSE.

# Positive association
isCOP.RTI(cop=PSP) # compute fast, has lower tail dependency
# TRUE

# Negative association Plackett
isCOP.RTI(cop=PLACKETTcop, para=c(.15))
# FALSE

# Positive association Plackett
isCOP.RTI(cop=PLACKETTcop, para=c(15))
# TRUE



# Negative association Plackett
isCOP.RTI(cop=PLACKETTcop, wrtv=TRUE, para=c(.15))
# FALSE

# Positive association Plackett
isCOP.RTI(cop=PLACKETTcop, wrtV=TRUE, para=c(15))
# TRUE
}
}
\keyword{distribution}
\keyword{multivariate}
