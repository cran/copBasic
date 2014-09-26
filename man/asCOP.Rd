\name{asCOP}
\alias{asCOP}
\title{ A Wrapper on a User-Level Formula to Become a Copula Function }
\description{
This function is intended to document and then to extend a simple API to end users to aid in implementation of other copulas for use in the \pkg{copBasic} package. There is no need (requirement) to use \code{asCOP} but for the mathematical definition of some copulas, the \code{asCOP} function might help considerably.  There is a need for special treatment of \eqn{u} and \eqn{v} vectors of probability as they  interact with the vectorization implicit in \R. The special treatment is needed because many copulas are based on the operators such as \code{min()} and \code{max()}. When numerical integration used by the \code{integrate()} function in \R in some copula operators, such as \code{\link{tauCOP}} for the Kendall's Tau of a copula, detailed accommodation of how \code{integrate()} works is needed because of \R's vectorization.

Basically, the problem is that one can not strictly rely on what \R does in terms of value recycling when  \eqn{u} and \eqn{v} are of unequal lengths. The source code is straightforward. Simply put, if lengths of \eqn{u} and \eqn{v} are unity, then there is no concern, and even if the length of \eqn{u} (say) is unity and \eqn{v} is 21, then recycling of \eqn{u} would often be ok. The real danger is when \eqn{u} and \eqn{v} have unequal lengths and those lengths are each greater than unity---\R's treatment can not be universally relied upon.

The example shows how a formula definition of a copula that is not a copula provided by \pkg{copBasic} is set into a function \code{deltacop} and then used inside another function \code{UsersCop} that will be the official copula that is compatible with a host of functions in \pkg{copBasic}. The use of \code{asCOP} provides the length check necessary on \eqn{u} and \eqn{v}, and the argument \code{...} provides optional parameter support should the user's formula require more settings.
}
\usage{
asCOP(u, v, f=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction; and}
  \item{f}{A function for which the user desires to make as a copula;}
  \item{...}{Additional arguments to pass to the function \code{f} (such as parameters, if needed, for the copula in the form of a list).}
}
\value{
  The value(s) for the copula are returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}}
\examples{
\dontrun{
# Concerning Nelson (2006, Example 5.14, p. 195)
"deltacop" <- function(u,v, ...) {   min(c(u,v,(u^2+v^2)/2))   }
"UsersCop" <- function(u,v, ...) { asCOP(u,v, f=deltacop, ...) }
isCOP.PQD(cop=UsersCop) # TRUE and Rho=0.288, Tau=0.333 as Nelson says
isCOP.LTD(cop=UsersCop, wrtV=TRUE) # FALSE as Nelson says
isCOP.RTI(cop=UsersCop, wrtV=TRUE) # FALSE as Nelson says
}
}
\keyword{copula (utilities)}
\keyword{copula (formulas)}

