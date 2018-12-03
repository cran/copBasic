\encoding{utf8}
\name{rmseCOP}
\alias{rmseCOP}
\title{ The Root Mean Square Error between a Fitted Copula and an Empirical Copula }
\description{
Compute the \emph{root mean square error} \eqn{\mathrm{RMSE}_\mathbf{C}} (Chen and Guo, 2019, p. 29), which is computed using \emph{mean square error} \eqn{\mathrm{MSE}} as

\deqn{\mathrm{MSE}_\mathbf{C} = \frac{1}{n}\sum_{i=1}^n \bigl(\mathbf{C}_n(u_i,v_i) - \mathbf{C}_{\Theta_n}(u_i, v_i)\bigr)^2\mbox{ and}}
\deqn{\mathrm{RMSE}_\mathbf{C} = \sqrt{\mathrm{MSE}_\mathbf{C}}\mbox{,}}

where \eqn{\mathbf{C}_n(u_i,v_i)} is the \emph{empirical copula} (empirical joint probability) for the \eqn{i}th observation, \eqn{\mathbf{C}_{\Theta_m}(u_i, v_i)} is the fitted copula having \eqn{m} parameters in \eqn{\Theta}. The \eqn{\mathbf{C}_n(u_i,v_i)} comes from \code{\link{EMPIRcop}}. The \eqn{\mathrm{RMSE}_\mathbf{C}} is in effect saying that the best copula will have its joint probabilities plotting on a 1:1 line with the empirical joint probabilities, which is an \eqn{\mathrm{RMSE}_\mathbf{C}=0}. From the \eqn{\mathrm{MSE}_\mathbf{C}} shown above, the Akaike information criterion (AIC) \code{\link{aicCOP}} and Bayesian information criterion (BIC) \code{\link{bicCOP}} can be computed, which add a penalty for \eqn{m} parameters. These goodness-of-fits can assist in deciding one copula favorability over another, and another goodness-of-fit using the absolute differences between \eqn{\mathbf{C}_n(u,v)} and \eqn{\mathbf{C}_{\Theta_m}(u, v)} is found under \code{\link{statTn}}.
}
\usage{
rmseCOP(u,v, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{...}{Additional arguments to pass to either copula (likely most commonly to the empirical copula).}
}
\value{
  The value for \eqn{\mathrm{RMSE}_\mathbf{C}} is returned.
}
\references{
Chen, Lu, and Guo, Shenglian, 2019, Copulas and its application in hydrology and water resources: Springer Nature, Singapore, ISBN 978--981--13--0574--0.
}
\author{ W.H. Asquith}
\seealso{\code{\link{EMPIRcop}}, \code{\link{aicCOP}}, \code{\link{bicCOP}}
}
\examples{
\dontrun{
S <- simCOP(80, cop=GHcop, para=5) # Simulate some probabilities, but we
# must then treat these as data and recompute empirical probabilities. 
U <- lmomco::pp(S$U, sort=FALSE); V <- lmomco::pp(S$V, sort=FALSE)
# The parent distribution is Gumbel-Hougaard extreme value copula.
# But in practical application we don't know that but say we speculate that
# perhaps the Galambos extreme value might be the parent. Then maximum
# likelihood is used to fit the single parameter.
pGL <- mleCOP(U,V, cop=GLcop, interval=c(0,20))$par

rmses <- c(rmseCOP(U,V, cop=GLcop, para=pGL),
           rmseCOP(U,V, cop=P),
           rmseCOP(U,V, cop=PSP))
print(rmses) # We will see that the first RMSE is the smallest as the
# Galambos has the nearest overall behavior than the P and PSP copulas.}
}
\keyword{copula (goodness-of-fit)}
\keyword{copula (inference)}
\keyword{inference}
\keyword{goodness-of-fit}
