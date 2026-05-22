\encoding{utf8}
\name{isfuncCOP}
\alias{isfuncCOP}
\title{Is a General Bivariate Function a Copula by Gridded Search?}
\description{
Is a general bivariate function a copula? Three properties are identified by Nelsen (2006, p. 10) for a bivariate copula \eqn{\mathbf{C}(u,v)}:
\deqn{\mathbf{C}(u,0) = 0 = \mathbf{C}(0,v)\mbox{\quad Nelsen 2.2.2a,}}
\deqn{\mathbf{C}(u,1) = u \mbox{\ and\ } \mathbf{C}(1,v) = v\mbox{\quad Nelsen 2.2.2b, and}}
for every \eqn{u_1, u_2, v_1, v_2} in \eqn{\mathcal{I}^2} such that \eqn{u_1 \le u_2} and \eqn{v_1 \le v_2},
\deqn{\mathbf{C}(u_2, v_2) - \mathbf{C}(u_2, v_1) - \mathbf{C}(u_1, v_2) + \mathbf{C}(u_1, v_1) \ge 0 \mbox{\quad Nelsen 2.2.2c.}}
The last condition is known also as \dQuote{2-increasing.} The \code{isfuncCOP} works along a gridded search in the domain \eqn{\mathcal{I}^2 = [0,1]\times[0,1]} for the 2-increasing check with a resolution \eqn{\Delta u = \Delta v} \eqn{=} \code{delta}. Because there are plenty of true copula functions available in the literature it seems unlikely that this function provides much production utility in investigations. This function is provided because part of the objectives of the \pkg{copBasic} package is for instructional purposes. The computational overhead is far too great for relative benefit to somehow dispatch to this function all the time using the other copula utilities in this package.
}
\usage{
isfuncCOP(cop=NULL, para=NULL, delta=0.002, ...)
}
\arguments{
  \item{cop}{A potential bivariate copula function that accepts two arguments for the \eqn{u} and \eqn{v} and parameters along argument \code{para} with option of additional arguments through the \code{...} argument;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{delta}{The \eqn{\Delta u = \Delta v} of the grid edges; and}
  \item{...}{Additional arguments to pass to the copula function.}
}
\value{
  A logical value of \code{TRUE} or \code{FALSE} is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{S. Kloibhofer (idea and most code) and W.H. Asquith (documentation)}
\seealso{\code{\link{densityCOP}}
}
\examples{
\dontrun{
"NelsenEx2.11" <- function(u,v, ...) { # Nelsen (2006, exer. 2.11, p. 16)
  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) return(NA)
  if(length(u) == 1) u <- rep(u, length(v))
  if(length(v) == 1) v <- rep(v, length(u))
  return(sapply(1:length(u), function(i) { upv <- u[i] + v[i]
                 if(2/3 <= upv & upv <= 4/3) return(min(c(u,v,1/3,upv-(2/3))))
                 max(u[i]+v[i]-1, 0) }))
}
isfuncCOP(cop=NelsenEx2.11) # FALSE}
}
\keyword{copula (properties)}
\keyword{Nelsen (2006) Examples and Exercises}
