\name{COP}
\alias{COP}
\title{ The Copula }
\description{
Compute the \emph{copula} or \emph{joint distribution function} from a copula function and as shown by Nelson (2006, p. 18) is the joint probability

\deqn{\mathrm{Pr}[U \le u, V \le v] = \mathbf{C}(u,v)\mbox{.}}

The copula is an expression of the joint probability that both \eqn{U \le u} and \eqn{V \le v}. A \emph{symmetric copula} is one such that \eqn{\mathbf{C}(u,v) = \mathbf{C}(v,u)} otherwise the copula is \emph{asymmetric}. The copula \emph{inversions} \eqn{t = \mathbf{C}(u=U, v)} or \eqn{t = \mathbf{C}(u, v=V)} are provided by \code{\link{COPinv}} and \code{\link{COPinv2}}, respectively. Perhaps this function is singularly the most important in \pkg{copBasic}.
}
\usage{
COP(u, v, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function with vectorization as in \code{asCOP};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{coCOP}}, \code{\link{duCOP}}, \code{\link{surCOP}}, \code{\link{surfuncCOP}} }
\examples{
u <- runif(1); v <- runif(1)
COP(cop=W,u,v); COP(cop=P,u,v); COP(cop=M,u,v); COP(cop=PSP,u,v)

F <- 0.75 # 75th percentile, nonexceedance
G <- 0.20 # 25th percentile, nonexceedance
bF <- 1 - F; bG <- 1 - G  # exceedance

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
COP(cop=PSP,F,G)      + coCOP(cop=PSP,bF,bG)
surCOP(cop=PSP,bF,bG) + duCOP(cop=PSP,F,G)

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

