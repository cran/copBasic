\encoding{utf8}
\name{simcomposite3COP}
\alias{simcomposite3COP}
\title{Compute the L-comoments of a Four-Value Composited Copula by Simulation}
\description{
Simulate copula parameters and compute \emph{L-comoments} and provision for plotting features for a composited copula using four compositing parameters (see \code{\link{composite3COP}}). The compositing parameters are each independent and uniformly distributed:
\deqn{\alpha \sim \mathrm{U}[0,1];\ \beta \sim \mathrm{U}[0,1];\ \kappa \sim \mathrm{U}[0,1];\ \gamma \sim \mathrm{U}[0,1]\mbox{.}}
L-comoment estimation is provided by the \code{\link{lcomCOP}}.
}
\usage{
simcomposite3COP(nsim=100, compositor=composite3COP,
                 parents=NULL, ploton=FALSE, points=FALSE,
                 showpar=FALSE, showresults=FALSE, digits=6, ...)
}
\arguments{
  \item{nsim}{Number of simulations to perform;}
  \item{compositor}{The compositing function that could be either \code{\link{composite1COP}}, \code{\link{composite2COP}}, and \code{\link{composite3COP}};}
  \item{parents}{A special parameter \code{list} (see \bold{Note});}
  \item{ploton}{A logical to toggle on intermediate plotting;}
  \item{points}{A logical to actually draw the simulations on the \code{ploton} by the \code{points()} function in \R;}
  \item{showpar}{Print the simulated parameter set with each iteration;}
  \item{showresults}{Print the results (useful if harvest results from a batch operation in \R);}
  \item{digits}{The number digits to pass to \code{round} if \code{showresults} is true; and}
  \item{...}{Additional arguments to pass.}
}
\value{
An \R matrix of results is returned. Each row represents a single simulation run. The first four columns are the \eqn{\alpha}, \eqn{\beta}, \eqn{\kappa}, and \eqn{\gamma} \emph{compositing parameters} and are labeled as such. The next two columns are the opposing diagonals, by first row and then second, of the \emph{L-comoment correlation}. The following two columns are the opposing diagonals, by row and then second, of the \emph{L-coskew}. The following two columns are the opposing diagonals, by row and then second, of the \emph{L-cokurtosis}. The L-comoment columns are labeled to reflect the L-comoment matrix: \code{T2.21} means the L-comoment correlation row 2 column 1 and \code{T3.12} mean the L-coskew row 1 column 2. The remaining columns represent the \eqn{\Theta_n} parameters for copula 1, the \eqn{\Theta_m} parameters for copula 2. The columns are labeled \code{Cop1Thetas} or \code{Cop2Thetas}.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.
}
\note{
The following descriptions list in detail the \code{parents} argument structure and content of the \code{para} argument:
\describe{
  \item{\code{cop1}}{--- Function of the first copula;}
  \item{\code{cop2}}{--- Function of the second copula;}
  \item{\code{para1gen}}{--- Function to generate random parameters for the first copula; and}
  \item{\code{para2gen}}{--- Function to generate random parameters for the second copula.}
}
The \code{para} argument of this function are passed to the function contained in \code{compositor} and are therefore subject to further constraints in items should such constraints exist.
}
\author{W.H. Asquith}
\seealso{\code{\link{lcomCOP}}, \code{\link{simcompositeCOP}}}
\examples{
\dontrun{
# EXAMPLE 1: Make a single simulation result.
mainpara <- list(cop1=PLACKETTcop, cop2=PLACKETTcop,
                 para1gen=function() { return(c(10^runif(1, min=-5, max=0))) },
                 para2gen=function() { return(c(10^runif(1, min= 0, max=5))) })
v <- simcompositeCOP(nsim=1, parent=mainpara, showresults=TRUE)
print(v)

# EXAMPLE 2: Make 1000 "results" and plot two columns.
mainpara <- list(cop1=PLACKETTcop, cop2=N4212cop,
                 para1gen=function() { return(c(10^runif(1, min=-5, max=5))) },
                 para2gen=function() { return(c(10^runif(1, min= 0, max=2))) })
v <- simcomposite3COP(nsim=100, parent=mainpara); labs <- colnames(v)
plot(v[,5], v[,7],           # open circles are 1 with respect to 2
     xlab=paste(c(labs[5], "and", labs[6]), collapse=" "),
     ylab=paste(c(labs[6], "and", labs[8]), collapse=" "))
points(v[,6], v[,8], pch=16) # black dots are 2 with respect to 1}
}
\keyword{copula composition}
\keyword{copula (simulation)}
\keyword{visualization}
\keyword{L-comoments}

