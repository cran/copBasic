\name{rhoCOP}
\alias{rhoCOP}
\title{ The Spearman's Rho of a Copula }
\description{
Compute the measure of association known as \emph{Spearman's Rho} \eqn{\rho_\mathbf{C}} of a copula according to Nelson (2006, pp. 167--170, 189, 208) by
\deqn{\rho_\mathbf{C} = 12\int_0^1 \int_0^1 \mathbf{C}(u,v)\, \mathrm{d}u\mathrm{d}v - 3\mbox{\ or}}
\deqn{\rho_\mathbf{C} = 12\int_0^1 \int_0^1 [\mathbf{C}(u,v) - uv]\, \mathrm{d}u\mathrm{d}v\mbox{,}}
where the later equation is implemented by \code{rhoCOP}. The integral
\deqn{\int_0^1 \int_0^1 \mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v\mbox{,}}
represents the \dQuote{volume under the graph of the copula and over the unit square} (Nelson, 2006, p. 170) and therefore \eqn{\rho_\mathbf{C}} is simple a rescaled volume under the copula. The second equation for \eqn{\rho_\mathbf{C}} expresses the \dQuote{average distance} between the joint distribution and pure independence \eqn{\mathbf{\Pi} = uv}. Nelson (2006, pp. 175--176) observes that the following relation between \eqn{\rho_\mathbf{C}} and \eqn{\tau_\mathbf{C}} (\code{\link{tauCOP}}) exists
\deqn{-1 \le 3\tau - 2\rho \le 1\mbox{.}}
}
\usage{
rhoCOP(cop=NULL, para=NULL, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force integration using \code{brute}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\rho_\mathbf{C}} is returned.
}
\note{
Technically, Nelson (2006) also shows that these definitions are a form of call to a \dQuote{concordance} function of two copulas \eqn{Q(\mathbf{C}_1,\mathbf{C}_2)} that involve \eqn{\mathbf{C}_1{=}\mathbf{C}(u,v)} and \eqn{\mathbf{C}_2{=}\mathbf{\Pi}}. As such in order to keep \code{rhoCOP} a small function when \code{brute=TRUE}, \eqn{\rho_\mathbf{C}} is computed by a special call to \code{\link{tauCOP}}, which itself, although titled for computation of Kendall's Tau, can be made to support the concordance function \eqn{Q(\mathbf{C}_1, \mathbf{C}_2)} (see Nelson (2006, pp. 158--159) when given two different copulas and respective parameters as arguments.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{giniCOP}}, \code{\link{hoefCOP}}, \code{\link{tauCOP}}, \code{\link{wolfCOP}}
}
\examples{
rhoCOP(cop=PSP)             # 0.4784176
\dontrun{
rhoCOP(cop=PSP, brute=TRUE) # 0.4684063
# CPU heavy example showing that the dual-integration (fast) results in
# a Spearman's Rho that mimics a sample version
dorho <- function(n) {
   uv <- simCOP(n=n, cop=PSP, ploton=FALSE, points=FALSE)
   return(cor(uv$U, uv$V, method="spearman"))
}
rhos <- replicate(100, dorho(1000))
rho.sample <- mean(rhos); print(rho.sample) # 0.472661
}
\dontrun{
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.00395,    para2=4.67, alpha=0.9392, beta=0.5699)
rhoCOP(cop=composite2COP, para=para) # -0.5924796

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.14147,    para2=20.96, alpha=0.0411, beta=0.6873)
rhoCOP(cop=composite2COP, para=para) # 0.2818874

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.10137,     para2=4492.87, alpha=0.0063, beta=0.0167)
rhoCOP(cop=composite2COP, para=para)             # 0.9812919
rhoCOP(cop=composite2COP, para=para, brute=TRUE) # 0.9752155
}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}

