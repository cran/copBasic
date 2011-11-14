\name{PlackettPlackettNP}
\docType{data}
\alias{PlackettPlackettNP}
\title{Parameters and L-comoments of a Composition of Two Plackett Copulas}
\description{
These \dQuote{data} contain the stochastically generated parameter space of a Plackett-Plackett composited copulas to support a fast lookup of the relation between four copula parameters and the four L-comoments of L-correlation and L-coskew---hence, support the copulatic method of L-comoments. The \dQuote{NP} in the name indicates that the first Plackett copula was only configured for negative or anticorrelation and the second Plackett copula was only configured for positive correlation. This particular data set is provided in the \pkg{copBasic} package for self-contained parameter estimation, but much much greater data sets are available from the author.

Specifically, these data contain the stochastically generated parameter space of the \eqn{\alpha} and \eqn{\beta} \dQuote{mixing} parameters of the composited copula in which the two copulas to composite \eqn{\mathbf{A}} and \eqn{\mathbf{B}} are set as Plackett types. The composited copula is defined as

\deqn{\mathbf{C}_{\alpha,\beta}(u,v) = \mathbf{A}(u^\alpha,    v^\beta) \cdot
                              \mathbf{B}(u^{1-\alpha},v^{1-\beta})\mbox{.}}

The first Plackett \eqn{\mathbf{A}} (\code{\link{PLACKETTcop}}) was randomly generated between the product copulas \eqn{\Pi} (\code{\link{P}}) and the Frechet-Hoeffding lower bound copula \eqn{\mathbf{W}} (\code{\link{W}}). The second Plackett \eqn{\mathbf{B}} (\code{\link{PLACKETTcop}}) was randomly generated between the product copulas \eqn{\Pi} (\code{\link{P}}) and the Frechet-Hoeffding upper bound copula \eqn{\mathbf{M}} (\code{\link{M}}). The \eqn{\mathbf{A}} and \eqn{\mathbf{B}} generated in this way means that the first copula controls the negative correlative aspects of a data set and the second copula controls the positive aspects.

To further clarify, \eqn{\mathbf{A}_\Theta} parameter is on the interval \eqn{[0,1]} and the \eqn{\mathbf{B}_\Theta} parameter is on the inteval \eqn{[1,\infty]}. In reality the \eqn{\Theta} parameters were generated uniformly in log-space and then transformed. The log of \eqn{\mathbf{A}_\Theta} was on the interval \eqn{[-5,0]}, and the log of \eqn{\mathbf{B}_\Theta} was on the interval \eqn{[0,5]}. (See such limits of generation in the Source section of this documentation.)
}
\usage{
data(PlackettPlackettNP)
}
\format{
A data frame with
\describe{
  \item{alpha}{The \eqn{\alpha} parameter of composited copula, }
  \item{beta}{The \eqn{\beta} parameter of composited copula,}
  \item{T2.12}{The L-correlation on row 1 and column 2 of matrix,}
  \item{T2.21}{The L-correlation on row 2 and column 1 of matrix,}
  \item{T3.12}{The L-coskew on row 1 and column 2 of matrix,}
  \item{T3.21}{The L-coskew on row 2 and column 1 of matrix,}
  \item{T4.12}{The L-cokurtosis on row 1 and column 2 of matrix,}
  \item{T4.21}{The L-cokurtosis on row 2 and column 1 of matrix,}
  \item{Theta1}{The \eqn{\Theta} parameter of first Plackett copula, and}
  \item{Theta2}{The \eqn{\Theta} parameter of second Plackett copula.}
}
}
\source{
This \dQuote{data} set was created using the \code{\link{simcompositeCOP}} function, which in turn defaults to use of the \code{\link{composite2COP}} function for compositing of the two Plackett copulas. The usage is shown below.
\preformatted{
mainpara <- list(cop1=PLACKETTcop,
                 cop2=PLACKETTcop,
                 para1gen=function() {
                    return(10^runif(1, min=-5, max=0)) },
                 para2gen=function() {
                    return(10^runif(1, min=0,  max=5)) })

n <- 10 # really used 20000 for generation of the data set
PlackettPlackettNP <-
    simcompositeCOP(n=2000, nsim=n, parent=mainpara)
save(PlackettPlackettNP,
     file="PlackettPlackettNP.RData", compress="xz")
}
For the generation process, the \code{mainpara} list specifies the two copulas to use in the composite, and two generation functions for the parameters of the respective copulas are housed in \code{para1gen} and \code{para2gen}. For each of 20000 simulations of size 500 of the stochastically generated Plackett-Plackett composited copulas, the L-correlation and L-coskew matrices are computed. The opposing diagonals of the L-correlation and L-coskew of the matrices are requested internally by \code{\link{simcompositeCOP}} and shown. The L-comoment matrices are computed by the \code{lcomoms2()} function of the \pkg{lmomco} package.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007,
Extremes in Nature---An approach using copulas: Springer, 289 p.

Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v. 98, pp. 1765--1781.
}
\examples{

\dontrun{
data(PlackettPlackettNP)
summary(PlackettPlackettNP)

PlackettPlackettNP <- as.data.frame(PlackettPlackettNP)
# Let us now visualize the parameter space
plot.complcom.space <- function(text=NULL) {
  U12 <- PlackettPlackettNP$T2.12
  U21 <- PlackettPlackettNP$T2.21
  V12 <- PlackettPlackettNP$T3.12
  V21 <- PlackettPlackettNP$T3.21
  U <- c(U12,U21); V <- c(V12,V21)
  plot(U,V, type="n", xlim=c(-1,1), ylim=c(-0.2,0.4),
       xlab="L-CORRELATION",
       ylab="L-COSKEW")
  # plot transparent blue L-correlation and L-coskew first
  points(U12,V12, col=rgb(   0, 0, 1, 0.12), pch=16)
  # plot transparent purple L-correlation and L-coskew second
  points(U21,V21, col=rgb(0.85, 0, 1, 0.12), pch=16)
  abline(v=0); abline(h=0) # cross lines
  mtext(text)
}
my.text <-
"L-COMOMENTS OF PLACKETT(negative)-PLACKETT(postive) COPULA"
plot.complcom.space(text=my.text) # plot the parameter space


# Let us now visualize the parameter space
plot.complcom.space2 <- function(text=NULL) {
  U12 <- PlackettPlackettNP$T3.12
  U21 <- PlackettPlackettNP$T3.21
  V12 <- PlackettPlackettNP$T4.12
  V21 <- PlackettPlackettNP$T4.21
  U <- c(U12,U21); V <- c(V12,V21)
  plot(U,V, type="n", xlim=c(-0.2,0.4), ylim=c(-0.2,0.2),
       xlab="L-COSKEW",
       ylab="L-COKURTOSIS")
  # plot transparent blue L-correlation and L-coskew first
  points(U12,V12, col=rgb(   0, 0, 1, 0.12), pch=16)
  # plot transparent purple L-correlation and L-coskew second
  points(U21,V21, col=rgb(0.85, 0, 1, 0.12), pch=16)
  abline(v=0); abline(h=0) # cross lines
  mtext(text)
}
my.text <-
"L-COMOMENTS OF PLACKETT(negative)-PLACKETT(postive) COPULA"
plot.complcom.space2(text=my.text) # plot the parameter space

}

}
\keyword{datasets}
