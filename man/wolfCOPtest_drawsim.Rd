\encoding{utf8}
\name{wolfCOPtest_drawsim}
\alias{wolfCOPtest_drawsim}
\title{Draw Simulations of Schweizer and Wolff Sigma Test for Data Having Ties or Censoring}
\description{
Draw simulations of the \emph{Schweizer--Wolff Sigma} test and make comparisons to the \emph{Kendall Tau} and \emph{Spearman Rho} in the context of ties and (or) the \code{zmat} given in the call to \code{\link{wolfCOPtest}}.
}
\usage{
wolfCOPtest_drawsim(swolf, add=FALSE, alpha=0.05, pdffile=NA,
                           title=NA, ulab="Us", vlab="Vs",
                           mai=c(0.75, 0.75, 0.50, 0.25), ...)
}
\arguments{
  \item{swolf}{The object from \code{\link{wolfCOPtest}} that must have the \code{table} list element from a trigger of the \code{aslist} argument either explicitly or by internal trip. It is necessary that \code{add.cor.tests} was set true;}
  \item{add}{If true, then the \code{plot()} infrastructure internally is not called but the \code{points()}, \code{lines()}, and \code{legend()} are;}
  \item{alpha}{Statistical significance level alpha to draw a horizontal line on the plot;}
  \item{pdffile}{Name for a pdf file to render if not missing;}
  \item{title}{An optional title of the plot;}
  \item{ulab}{A string to insert in an informational measure of number of unique values in the horizontal \code{U} direction below the \code{title}. If \code{ulab} and \code{vlab} are both set missing, then the informational measure is not labeled about the plot;}
  \item{vlab}{A string to insert in an informational measure of number of unique values in the vertical \code{V} direction below the \code{title}. If \code{ulab} and \code{vlab} are both set missing, then the informational measure is not labeled about the plot;}
  \item{mai}{Settings for an explicit call to the \code{mai} of \code{par()}; and}
  \item{...}{Additional arguments to pass (refer to source code).}
}
\value{
  This function is used for its graphical effects.
}
\author{W.H. Asquith}
\seealso{\code{\link{wolfCOPtest}}
}
\examples{
\dontrun{
  # Example of complete ties in X and Y but the simulation.
  n <- 30; X <- Y <- rep(10, n)
  S <- wolfCOPtest(X,Y, ties.method="random", add.cor.tests=TRUE, rndphi=100)
  wolfCOPtest_drawsim(S); w <- seq(0, 1, by=0.002)
  p <- sapply(w, function(w) wolfCOPtest(w, n)$p.value[2] )
  lines(w, p, lty=2, lwd=2, col="firebrick") # This line plots on top of prior black
  # line that is a requirement if all the algorithms are working as advertised. }
}
\keyword{Schweizer and Wolff Sigma}
\keyword{Schweizer and Wolff Sigma (hypothesis test)}
\concept{Hypothesis test for Schweizer and Wolff Sigma}
