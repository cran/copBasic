\name{simcompositeCOP}
\alias{simcompositeCOP}
\title{ Simulation of a Composited Copula }
\description{
This function simulates and provides plotting features for a composited copula through composition by various methods.
}
\usage{
simcompositeCOP(n=1000, nsim=100, compositor=composite2COP,
                parents=NULL, ploton=FALSE, points=FALSE,
                showpar=FALSE, showresults=FALSE, digits=6, ...)
}
\arguments{
  \item{n}{Sample size per simulation,}
  \item{nsim}{Number of simulations to perform,}
  \item{compositor}{The compositing function, could be \code{\link{composite1COP}} or \code{\link{composite2COP}},}
  \item{parents}{A special parameter \code{list()} (see Note),}
  \item{ploton}{A logical to toggle on intermediate plotting,}
  \item{points}{A logical to actually draw the simulations on the \samp{ploton} by \code{points()},}
  \item{showpar}{Print the simulated parameter set with each iteration,}
  \item{showresults}{Print the results (useful if harvest results from a batch operation of R),}
  \item{digits}{The number digits to pass to \samp{round} if \samp{showresults} is true, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A matrix of results is returned. Each row represents a simulation run. The first two columns are the \eqn{\alpha} and \eqn{\beta} parameters and are labeled as such. The next two columns are the opposing diagonals, by first row and then second, of the L-comoment correlation. The following two columns are the opposing diagnonals, by row and then second, of the L-coskew. The L-comoment columns are labeled as such, \code{T2.21} means the L-comoment correlation row 2 column 1 and \code{T3.12} mean the L-coskew row 1 column 2. The remaining columns represent the \eqn{n} parameters for copula 1, the \eqn{m} parameters for copula 2. The columns are labeled \code{Cop1Thetas} or \code{Cop2Thetas}.
}
\note{
The following descriptions list in detail the \samp{parents} argument structure and content of the \code{para} argument:
\describe{
  \item{\samp{cop1}}{Function of the first copula,}
  \item{\samp{cop2}}{Function of the second copula,}
  \item{\samp{para1gen}}{Function to generate random parameters for the first copula, and}
  \item{\samp{para2gen}}{Function to generate random parameters for the second copula.}
}

The \code{para} argument of this function are passed to the function contained in \code{compositor} and are therefore subject to further constraints in items should such constraints exist.
}
\author{ W.H. Asquith}
\seealso{\code{\link{composite1COP}}, \code{\link{composite2COP}}}
\examples{
mainpara <- list(cop1=PLACKETTcop,
                 cop2=PLACKETTcop,
                 para1gen=function() {
                       return(c(10^runif(1,min=-5,max=0))) },
                 para2gen=function() {
                       return(c(10^runif(1,min=0,max=5))) })

v <- simcompositeCOP(n=200, nsim=1, parent=mainpara, showresults=TRUE)
print(v)
}
\keyword{distribution}
\keyword{multivariate}
