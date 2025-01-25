\encoding{utf8}
\name{aicCOP}
\alias{aicCOP}
\title{Akaike Information Criterion between a Fitted Coupla and an Empirical Copula}
\description{
Compute the \emph{Akaike information criterion} (AIC) \eqn{\mathrm{AIC}_\mathbf{C}} (Chen and Guo, 2019, p. 29), which is computed using \emph{mean square error} \eqn{\mathrm{MSE}_\mathbf{C}} as

\deqn{\mathrm{MSE}_\mathbf{C} = \frac{1}{n}\sum_{i=1}^n \bigl(\mathbf{C}_n(u_i,v_i) - \mathbf{C}_{\Theta_m}(u_i, v_i)\bigr)^2\mbox{ and}}
\deqn{\mathrm{AIC}_\mathbf{C} = 2m + n\log(\mathrm{MSE}_\mathbf{C})\mbox{,}}

where \eqn{\mathbf{C}_n(u_i,v_i)} is the \emph{empirical copula} (empirical joint probability) for the \eqn{i}th observation, \eqn{\mathbf{C}_{\Theta_m}(u_i, v_i)} is the fitted copula having \eqn{m} parameters in \eqn{\Theta}. The \eqn{\mathbf{C}_n(u_i,v_i)} comes from \code{\link{EMPIRcop}}. The \eqn{\mathrm{AIC}_\mathbf{C}} is in effect saying that the best copula will have its joint probabilities plotting on a 1:1 line with the empirical joint probabilities, which is an \eqn{\mathrm{AIC}_\mathbf{C} = -\infty}. From the \eqn{\mathrm{MSE}_\mathbf{C}} shown above, the root mean square error \code{\link{rmseCOP}} and Bayesian information criterion (BIC) \code{\link{bicCOP}} can be computed. These goodness-of-fits can assist in deciding one copula favorability over another, and another goodness-of-fit using the absolute differences between \eqn{\mathbf{C}_n(u,v)} and \eqn{\mathbf{C}_{\Theta_m}(u, v)} is found under \code{\link{statTn}}.
}
\usage{
aicCOP(u, v=NULL, cop=NULL, para=NULL, m=NA, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction; If not given, then a second column from argument \code{u} is attempted;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
    \item{m}{The number of parameters in the copula, which is usually determined by length of \code{para} if \code{m=NA}, but some complex compositions of copulas are difficult to authoritatively probe for total parameter lengths and mixing coefficients; and}
  \item{...}{Additional arguments to pass to either copula (likely most commonly to the empirical copula).}
}
\value{
  The value for \eqn{\mathrm{AIC}_\mathbf{C}} is returned.
}
\references{
Chen, Lu, and Guo, Shenglian, 2019, Copulas and its application in hydrology and water resources: Springer Nature, Singapore, ISBN 978--981--13--0574--0.
}
\author{W.H. Asquith}
\seealso{\code{\link{EMPIRcop}}, \code{\link{bicCOP}}, \code{\link{rmseCOP}}
}
\examples{
\dontrun{
  S <- simCOP(80, cop=GHcop, para=5) # Simulate some probabilities, but we
  # must then treat these as data and recompute empirical probabilities.
  U <- lmomco::pp(S$U, sort=FALSE); V <- lmomco::pp(S$V, sort=FALSE)
  # The parent distribution is Gumbel-Hougaard extreme value copula, but in practical
  # applications, we do not know that. Say we speculate that perhaps the Galambos extreme
  # value might be the parent; maximum likelihood is used to fit the single parameter.
  pGL  <- mleCOP(  U,V, cop=GLcop, interval=c(0, 20))$par
  aics <- c(aicCOP(U,V, cop=GLcop, para=pGL), aicCOP(U,V, cop=P), aicCOP(U,V, cop=PSP))
  names(aics) <- c("GLcop", "P", "PSP")
  print(aics) # We will see that the first AIC is the smallest because the
  # Galambos has the nearest overall behavior than the P and PSP copulas.}
}
\keyword{copula (goodness-of-fit)}
\keyword{copula (inference)}
\keyword{inference}
\keyword{goodness-of-fit}
\keyword{Akaike Information Criterion}
