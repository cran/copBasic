\name{COP}
\alias{COP}
\title{ The Copula }
\description{
Compute the copula from a copula 

\deqn{\mathrm{Pr}[U \le u, V \le v] = \mathbf{C}(u,v)}

The copula is an expression of the probability that both \eqn{U \le u} and \eqn{V \le v}.
}
\usage{
COP(cop=NULL, u, v, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{u}{An nonexceedance probability in X direction,}
  \item{v}{An nonexceedance probability in Y direction, and}
  \item{...}{Additional arguments to pass (such as parameters, if needed, for the copula in the form of a list).}
}
\value{
  The value for the copula is returned.
}
\note{
See p.~18 of the reference.  \code{COP} + \code{coCOP} equal unity and \code{surCOP} + \code{duCOP} equal unity.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{coCOP}}, \code{\link{surCOP}}, \code{\link{duCOP}}, \code{\link{W}}, \code{\link{P}}, \code{\link{M}}, \code{\link{PSP}}}
\examples{
u <- runif(1)
v <- runif(1)
COP(cop=W,u,v)
COP(cop=P,u,v)
COP(cop=M,u,v)
COP(cop=PSP,u,v)

F <- 0.75 # 75th percentile, nonexceedance
G <- 0.20 # 25th percentile, nonexceedance
bF <- 1 - F # nonexceedance
bG <- 1 - G # nonexceedance

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
\keyword{distribution}
\keyword{multivariate}
