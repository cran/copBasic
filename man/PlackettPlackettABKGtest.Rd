\encoding{utf8}
\name{PlackettPlackettABKGtest}
\docType{data}
\alias{PlackettPlackettABKGtest}
\title{Parameters and L-comoments of a Composition of Two Plackett Copulas with Four Compositing Parameters}
\description{
These \dQuote{data} contain the stochastically generated parameter space of a Plackett-Plackett composited copulas to support a fast lookup of the relation between four compositing copula parameters and the four L-comoments of L-correlation, L-coskew, and L-kurtosis---hence, support the \emph{copulatic method of L-comoments}. This particular data set is provided in the \pkg{copBasic} package for experimental self-contained parameter estimation.

Specifically, these data contain the stochastically generated parameter space of the \eqn{\alpha}, \eqn{\beta}, \eqn{\kappa}, and \eqn{\gamma} \dQuote{mixing} or \emph{compositing parameters} of the composited copula in which the two copulas for composition \eqn{\mathbf{A}} and \eqn{\mathbf{B}} are set as \emph{Plackett copulas} (see \code{\link{PLACKETTcop}}).



The construction of an asymmetric copula from (extended) composition two copulas provides for more sophisticated structures of dependence between variables. Let \eqn{\mathbf{A}} and \eqn{\mathbf{B}} be copulas with their own parameter sets. The composition is made by

\deqn{\mathbf{C}_{\alpha,\beta,\kappa,\gamma}(u,v) = u^\kappa v^\gamma
                              \mathbf{A}([u^{1-\kappa}]^\alpha,    [v^{1-\gamma}]^\beta) \cdot
                              \mathbf{B}([u^{1-\kappa}]^{1-\alpha},[v^{1-\gamma}]^{1-\beta})\mbox{,}}
defines a family of copulas \eqn{\mathbf{C}_{\alpha,\beta,\kappa,\gamma}}, with compositing parameters \eqn{\alpha,\beta,\kappa,\gamma \in \mathcal{I}:[0,1]}.

The first Plackett copula named \eqn{\mathbf{A}} (\code{\link{PLACKETTcop}}) was randomly generated between the \emph{\enc{Fréchet}{Frechet}-Hoeffding lower bound copula} \eqn{\mathbf{W}(u,v)} (see \code{\link{W}}) and the \emph{\enc{Fréchet}{Frechet}-Hoeffding upper bound copula} \eqn{\mathbf{M}(u,v)} (see \code{\link{M}}). The second Plackett copula named \eqn{\mathbf{B}} (\code{\link{PLACKETTcop}}) was randomly generated in a similar fashion as the copula in \eqn{\mathbf{A}}.

To further clarify, \eqn{\mathbf{A}_\Theta} parameter is on the interval \eqn{[0,\infty]} and the \eqn{\mathbf{B}_\Theta} parameter is on the inteval \eqn{[0,\infty]}. In reality, the \eqn{\Theta} parameters were generated uniformly in log-space and then transformed. The base-10 log of \eqn{\mathbf{A}_\Theta} was on the interval \eqn{[-5,5]}, and the log of \eqn{\mathbf{B}_\Theta} was on the interval \eqn{[-5,5]}. (See such limits of generation in the Source section of this documentation.)
}
\usage{
data(PlackettPlackettABKGtest)
}
\format{
An \R \code{data.frame} with
\describe{
  \item{alpha}{The \eqn{\alpha} compositing parameter;}
  \item{beta}{The \eqn{\beta} compositing parameter;}
  \item{gamma}{The \eqn{\gamma} compositing parameter;}
  \item{kappa}{The \eqn{\kappa} compositing parameter;}
  \item{T2.12}{The L-correlation on row 1 and column 2 of matrix;}
  \item{T2.21}{The L-correlation on row 2 and column 1 of matrix;}
  \item{T3.12}{The L-coskew on row 1 and column 2 of matrix;}
  \item{T3.21}{The L-coskew on row 2 and column 1 of matrix;}
  \item{T4.12}{The L-cokurtosis on row 1 and column 2 of matrix;}
  \item{T4.21}{The L-cokurtosis on row 2 and column 1 of matrix;}
  \item{Theta1}{The \eqn{\Theta} parameter of first Plackett copula \eqn{\mathbf{A}}, and}
  \item{Theta2}{The \eqn{\Theta} parameter of second Plackett copula \eqn{\mathbf{B}}.}
}
}
\source{
This \dQuote{data} set was created using the \code{\link{simcompositeCOP}} function, which in turn defaults to use of the \code{\link{composite2COP}} function for compositing of the two Plackett copulas. The usage is shown below.
\preformatted{
mainpara <- list(cop1=PLACKETTcop, cop2=PLACKETTcop,
                 para1gen=function() { return(10^runif(1, min=-5, max=5)) },
                 para2gen=function() { return(10^runif(1, min=-5, max=5)) })
nsim <- 20500
sample.size.for.estimation <- 1000
PlackettPlackettABKGtest <-
    simcomposite3COP(n=sample.size.for.estimation, nsim=nsim, parent=mainpara)
save(PlackettPlackettABKGtest, file="PlackettPlackettABKG.RData", compress="xz")
}
For the generation process, the \code{mainpara} list specifies the two copulas to use in the composite, and two generation functions for the parameters of the respective copulas are housed in \code{para1gen} and \code{para2gen}. For each of 20,500 simulations of size \eqn{n=1{,}000} of the stochastically generated Plackett-Plackett composited copulas, the L-correlation, L-coskew, and L-cokurtosis matrices were computed. The opposing diagonals of the L-correlation and L-coskew of the matrices were requested internally by \code{\link{simcompositeCOP}} and are shown. The L-comoment matrices were computed by the \code{lcomoms2(nmom=4)} function of the \pkg{lmomco} package.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.

Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v. 98, pp. 1765--1781.
}
\examples{

\dontrun{
data(PlackettPlackettABKGtest)
summary(PlackettPlackettABKGtest)

PlackettPlackettABKG <- as.data.frame(PlackettPlackettABKGtest)
# Let us now visualize the parameter space
plot.complcom.space <- function(text=NULL) {
  U12 <- PlackettPlackettABKG$T2.12; U21 <- PlackettPlackettABKG$T2.21
  V12 <- PlackettPlackettABKG$T3.12; V21 <- PlackettPlackettABKG$T3.21
  U <- c(U12, U21); V <- c(V12, V21)
  plot(U,V, type="n", xlim=c(-1,1), ylim=c(-0.2,0.4),
       xlab="L-CORRELATION", ylab="L-COSKEW")
  # plot transparent blue L-correlation and L-coskew first
  points(U12,V12, col=rgb(   0, 0, 1, 0.12), pch=16)
  # plot transparent purple L-correlation and L-coskew second
  points(U21,V21, col=rgb(0.85, 0, 1, 0.12), pch=16)
  abline(v=0); abline(h=0) # cross lines
  mtext(text)
}
my.text <- "L-COMOMENTS OF ABGK COMPOSITED PLACKETT-PLACKETT COPULA"
plot.complcom.space(text=my.text) # plot the parameter space

# Let us now visualize the parameter space
plot.complcom.space2 <- function(text=NULL) {
  U12 <- PlackettPlackettABKG$T3.12; U21 <- PlackettPlackettABKG$T3.21
  V12 <- PlackettPlackettABKG$T4.12; V21 <- PlackettPlackettABKG$T4.21
  U <- c(U12, U21); V <- c(V12, V21)
  plot(U,V, type="n", xlim=c(-0.2,0.4), ylim=c(-0.2,0.2),
       xlab="L-COSKEW", ylab="L-COKURTOSIS")
  # plot transparent blue L-correlation and L-coskew first
  points(U12,V12, col=rgb(   0, 0, 1, 0.12), pch=16)
  # plot transparent purple L-correlation and L-coskew second
  points(U21,V21, col=rgb(0.85, 0, 1, 0.12), pch=16)
  abline(v=0); abline(h=0) # cross lines
  mtext(text)
}
my.text <- "L-COMOMENTS OF ABKG COMPOSITED PLACKETT-PLACKETT COPULA"
plot.complcom.space2(text=my.text) # plot the parameter space
}
}
\keyword{datasets}
