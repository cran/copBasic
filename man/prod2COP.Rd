\encoding{utf8}
\name{prod2COP}
\alias{prod2COP}
\title{ The Product of Two Copulas }
\description{
Perform copula multiplication (so-called \dQuote{\eqn{\ast}-product} or \emph{Markov Product}) is a continuous analog of matrix multiplication and yields another copula:
\deqn{\bigl(\mathbf{C}_1 \ast \mathbf{C}_2 \bigr)(u,v) = \mathbf{C}_3(u,v) = \int_\mathcal{I} \frac{\delta \mathbf{C}_1(u, t)}{\delta v} \frac{\delta \mathbf{C}_2(t, v)}{\delta u}\,\mathrm{d}t\mbox{,}}
for copulas \eqn{\mathbf{C}_1(u, v)} and \eqn{\mathbf{C}_2(u, v)} are copulas whose \eqn{\ast}-product yields copula \eqn{\mathbf{C}_3(u, v)} in terms of partial derivatives (\code{\link{derCOP}} and \code{\link{derCOP2}}) of the other two. Nelsen (2006, p. 245) lists several identities of the \eqn{\ast}-product involving the product (\eqn{\mathbf{\Pi}}; \code{\link{P}}), lower bound (\eqn{\mathbf{W}}; \code{\link{W}}), and upper bound (\eqn{\mathbf{M}}; \code{\link{M}}) copulas:
\deqn{\mathbf{\Pi} \ast \mathbf{C} = \mathbf{C} \ast \mathbf{\Pi} = \mathbf{\Pi}\mbox{,}}
\deqn{\mathbf{M} \ast \mathbf{C} = \mathbf{C} \ast \mathbf{M} = \mathbf{M}\mbox{,}}
\deqn{\bigl(\mathbf{W} \ast \mathbf{C}\bigr)(u,v) = v - \mathbf{C}(1-u, v)\mbox{\ and\ } \bigl(\mathbf{C} \ast \mathbf{W}\bigr)(u,v) = u - \mathbf{C}(u, 1-v)\mbox{, and}}
\deqn{\mathbf{W} \ast \mathbf{W} = \mathbf{M}\mbox{ and } \mathbf{W} \ast \mathbf{C} \ast \mathbf{W} = \hat\mathbf{C}\mbox{,}}
where \eqn{\hat\mathbf{C}} is the \emph{survival copula} (\code{\link{surCOP}}). The \eqn{\ast}-product is associative:
\deqn{\mathbf{A} \ast (\mathbf{B} \ast \mathbf{C}) = (\mathbf{A} \ast \mathbf{B}) \ast \mathbf{C}\mbox{,}}
but \eqn{\ast}-product is not commutative (order independent). Nelsen (2006, p. 245) reports that \dQuote{if we view \eqn{\ast} as a binary operation on the set of copulas, then \eqn{\mathbf{\Pi}} is the null element, and \eqn{\mathbf{M}} is the identity.} Copula mulitiplication is closely linked to Markov Processes (Nelsen, 2006, pp. 244--248).

For other descriptions and computations of copula combination are possible using the \pkg{copBasic} package, see \code{\link{convex2COP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, and \code{\link{composite3COP}}.
}
\usage{
prod2COP(u,v, cop1=NULL, para1=NULL, cop2=NULL, para2=NULL, para=NULL,
              interval=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop1}{The \eqn{\mathbf{C}_1(u,v; \Theta_1)} copula function with vectorization as in \code{asCOP};}
  \item{para1}{Vector of parameters or other data structures for \eqn{\Theta_1}, if needed, to pass to copula \eqn{\mathbf{C}_1(u,v; \Theta_1)};}
  \item{cop2}{The \eqn{\mathbf{C}_2(u,v; \Theta_2)} copula function with vectorization as in \code{asCOP};}
  \item{para2}{Vector of parameters or other data structures for \eqn{\Theta_2}, if needed, to pass to copula \eqn{\mathbf{C}_2(u,v; \Theta_2)};}
  \item{para}{An \R \code{list} that can take the place of the \code{cop1}, \code{para1}, \code{cop2}, and \code{para2} arguments. These four will be populated from same named elements of the \code{list}, and if the other four arguments were specified through the function interface, these are silently ignored;}
  \item{interval}{An optional interval for the above integral. The default is \eqn{\mathcal{I} = [0,1]} but the option of the user to replace exact end points with \dQuote{small} numbers is possible (\emph{e.g.} \code{interval=}\code{c(lo, 1-lo)} for say \code{lo=.Machine$double.eps}; and)}
  \item{...}{Additional arguments to pass to the copulas.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
The \emph{Farlie--Gumbel--Morgenstern copula} \eqn{\mathbf{FGM}(u,v; \Theta)} is
\deqn{\mathbf{FGM}(u,v; \Theta) = uv[1+\Theta(1-u)(1-v)]\mbox{,}}
where \eqn{-1 \le \Theta \le 1}. Nelsen (2006, exer. 6.12, p. 249) asserts that for \eqn{\mathbf{FGM}_\alpha} and \eqn{\mathbf{FGM}_\beta} with \eqn{\ast}-product as \eqn{\mathbf{FGM}_\alpha \ast \mathbf{FGM}_\beta} that a closed-form solution exists and is
\deqn{\mathbf{FGM}_\alpha \ast \mathbf{FGM}_\beta = \mathbf{FGM}_{\alpha\beta/3}\mbox{.}}
That the assertion is numerically true as readily verified using the \code{prod2COP} function:
\preformatted{
  u <- c(0.41, 0.87); v <- c(0.13,0.35); A <- -0.532; B <- 0.235
  FGMcop(  u,v, para=A*B/3) # 0.05215986 and 0.30342773, matched by prod2COP()
  prod2COP(u,v, cop1=FGMcop, para1=A, cop2=FGMcop, para2=B)
}
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{convex2COP}}, \code{\link{composite1COP}},
                         \code{\link{composite2COP}}, \code{\link{composite3COP}} }
\examples{
\dontrun{
# Product P * N4212 ---> P (by identity)
u <- c(0.41, 0.87); v <- c(0.13,0.35)
prod2COP(u,v, cop1=P, cop2=N4212cop, para1=NA, para2=2.12) # 0.0533 and 0.3045
COP(u,v, cop=P)                                            # 0.0533 and 0.3045}

\dontrun{
para <- list(cop1=PLcop, para1=0.19, cop2=PLcop, para2=34.5)
UV <- simCOP(n=1000, cop=prod2COP, para=para)
# This is large simulation run (with a lot of numerical operations) is expected
# at least for the Placketts and chosen parameters to trigger one or more NAs
# from derCOPinv().  The simCOP() function simply continues on with ignoring the
# solution or lack thereof for certain combinations, and simCOP() will report how
# many of the simulated values for sample of size n were computed. For example,
# for one n=1000, some 965 simulated values were returned. The Spearman Rho is
rhoCOP(cop=prod2COP, para=para) # -0.4271195 (theoretical)
rhoCOP(para=UV, as.sample=TRUE) # -0.4284223 (the test n=1000 --> n=965 sample)}
}
\keyword{copula operator}
\concept{copula multiplication}
\concept{copula product}
\concept{Markov Product}
\keyword{Nelsen (2006) Examples and Exercises}
