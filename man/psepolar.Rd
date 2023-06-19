\encoding{utf8}
\name{psepolar}
\alias{psepolar}
\title{Pseudo-Polar Representation of Bivariate Data}
\description{
Kiriliouk \emph{et al.} (2016, pp. 358--360) describe a \emph{pseudo-polar} representation of bivariate data as a means to explore right-tail extremal dependency between the variables. Let \eqn{(X_i, Y_i)} (real values) or \eqn{(U_i, V_i)} (as probabilities) for \eqn{i = 1, \ldots, n} be a bivariate sample of size \eqn{n}. When such data are transformed into a \dQuote{unit-Pareto} scale by
\deqn{\widehat{X}^\star_i = n/(n+1-R_{X,i}) \mbox{\ and\ } \widehat{Y}^\star_i = n/(n+1-R_{Y,i})\mbox{,}}
where \eqn{R} is \code{rank()}, then letting each \emph{component sum} or \emph{pseudo-polar radius} be defined as
\deqn{\widehat{S}_i = \widehat{X}^\star_i + \widehat{Y}^\star_i\mbox{,}}
and each respective \emph{pseudo-polar angle} be defined as 
\deqn{\widehat{W}_i = \widehat{X}^\star_i / (\widehat{X}^\star_i + \widehat{Y}^\star_i) = \widehat{X}^\star_i / \widehat{S}_i\mbox{,}}
a pseudo-polar representation is available for study.

A scatter plot of \eqn{\widehat{W}_i} (horizontal) versus \eqn{\widehat{S}_i} (vertical) will depict a \emph{pseudo-polar plot} of the data. Kiriliouk \emph{et al.} (2016) approach the pseudo-polar concept as a means to study extremal dependency in the sense of what are the contributions of the \eqn{X} and \eqn{Y} to their sum conditional on the sum being large. The \emph{largeness} of \eqn{\widehat{S}_i} is assessed by its empirical cumulative distribution function and a threshold \eqn{S_f} stemming from \eqn{f} as a nonexceedance probability \eqn{f \in [0,1]}.

A density plot of the \eqn{\widehat{W}_i} is a representation of extremal dependence. If the density plot shows low density for pseudo-polar angles away from 0 and 1 or bimodality on the edges then weak extremal dependency is present. If the density is substantial and uniform away from the the angles 0 and 1 or if the density peaks near \eqn{\widehat{W} \approx 0.5} then extremal dependency is strong.
}
\usage{
psepolar(u, v=NULL, f=0.90, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction (actually the ranks are used so this can be a real-value argument as well);}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction  (actually the ranks are used so this can be a real-value argument as well) and if \code{NULL} then \code{u} is treated as a two column \R \code{data.frame};}
  \item{f}{The nonexceedance probability of the distal \eqn{\widehat{S}} to flag in \code{Shat_ge_Sf} column of the output; and}
  \item{...}{Additional arguments to pass to the \code{dat2bernqua()} function of the \pkg{lmomco} package.}
}
\value{
  An \R \code{data.frame} is returned in the \code{table} element and the \eqn{S_f} is in the \code{Sf} element.
  \item{U}{An echo of the \code{u} input;}
  \item{V}{An echo of the \code{v} input;}
  \item{Xstar}{The \eqn{\widehat{X}^\star_i} (Kiriliouk \emph{et al.}, 2016, eq. 17.8, p. 359);}
  \item{Ystar}{The \eqn{\widehat{Y}^\star_i} (Kiriliouk \emph{et al.}, 2016, eq. 17.8, p. 359);}
  \item{FXhat1}{The \eqn{F_{X,i} = 1 - 1/X^\star_i}, which is the inverse of Kiriliouk \emph{et al.} (2016, eq. 17.1, p. 354);}
  \item{FYhat1}{The \eqn{F_{Y,i} = 1 - 1/Y^\star_i}, which is the inverse of Kiriliouk \emph{et al.} (2016, eq. 17.1, p. 354);}
  \item{FXhat3}{The \eqn{F_{3,X,i} = (R_{X,i} - 0.5)/n} corresponding to the \dQuote{3} alternative identified by Kiriliouk \emph{et al.} (2016, p. 365);}
  \item{FYhat3}{The \eqn{F_{3,Y,i} = (R_{Y,i} - 0.5)/n} corresponding to the \dQuote{3} alternative identified by Kiriliouk \emph{et al.} (2016, p. 365);}
  \item{What}{The \eqn{\widehat{W}_i} (Kiriliouk \emph{et al.}, 2016, eq. 17.9, p. 359);}
  \item{Shat}{The \eqn{\widehat{S}_i} (Kiriliouk \emph{et al.}, 2016, eq. 17.9, p. 359); and}
  \item{Shat_ge_Sf}{A logical on whether the \eqn{\widehat{S}_i} are larger than \eqn{S_f}.}
}
\note{
The default of \code{f=0.90} means that the upper 90th percentile of the component sum will be identified in the output. This percentile is computed by the Bernstein empirical distribution function provided by the \pkg{lmomco} package through the \code{dat2bernqua()} function. Suggested arguments for \code{...} are \code{poly.type="Bernstein"} and \code{bound.type="Carv"} though the former is redundant because it is the default of \code{dat2bernqua()}.
}
\references{
Kiriliouk, Anna, Segers, Johan, \enc{Warchoł}{Warchol}, \enc{Michał}{Michal}, 2016, Nonparameteric estimation of extremal dependence: \emph{in} Extreme Value Modeling and Risk Analysis, D.K. Dey and Jun Yan \emph{eds.}, Boca Raton, FL, CRC Press, ISBN 978--1--4987--0129--7.
}
\author{William Asquith \email{william.asquith@ttu.edu}}
\seealso{\code{\link{spectralmeas}}, \code{\link{stabtaildepf}}
}
\examples{
\dontrun{
pse <- psepolar(simCOP(n=799, cop=PARETOcop, para=4.3,graphics=FALSE),bound.type="Carv")
pse <- pse$table # The Pareto copula has right-tail extreme dependency
plot(1/(1-pse$U), 1/(1-pse$V), col=pse$Shat_ge_Sf+1, lwd=0.8, cex=0.5, log="xy", pch=16)
plot(pse$What, pse$Shat, log="y", col=pse$Shat_ge_Sf+1, lwd=0.8, cex=0.5, pch=16)
plot(density(pse$What[pse$Shat_ge_Sf]), pch=16, xlim=c(0,1)) # then try the
# non-right tail extremal copula PSP as cop=PSP in the above psepolar() call.}
}
\keyword{copula (goodness-of-fit)}
\keyword{copula (inference)}
\keyword{inference}
\keyword{goodness-of-fit}
\keyword{pseudo-polar representation}
\concept{polar}
