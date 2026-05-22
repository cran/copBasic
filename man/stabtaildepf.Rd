\encoding{utf8}
\name{stabtaildepf}
\alias{stabtaildepf}
\title{Estimation of the Stable Tail Dependence Function}
\description{
Kiriliouk \emph{et al.} (2016, pp. 364--366) describe a technique for estimation of a \emph{empirical stable tail dependence function} for a random sample. The function is defined as
\deqn{\widehat{l}(x,y) = \frac{1}{k}\sum_{i=1}^n \mathbf{1}\bigl[ R_{i,x,n} > n + 1 - kx \mbox{\ or\ } R_{i,y,n} > n + 1 - ky \bigr]\mbox{,}}
where \eqn{\mathbf{1}[\cdot]} is an \emph{indicator function}, \eqn{R} denotes the \code{rank()} of the elements and \eqn{k \in [1,\ldots,n]} and \eqn{k} is intended to be \dQuote{large enough} that \eqn{\widehat{l}(x,y)} has converged to a limit.

The \dQuote{\enc{Capéraà}{Caperaa}--\enc{Fougères}{Fougeres} smooth} of the empirical stable tail dependence function is defined for a coordinate pair \eqn{(x,y)} as
\deqn{\widehat{l}_{CF}(x,y) = 2 \sum_{i \in I_n} \widehat{p}_{3,i} \times \mathrm{max}\bigl[\widehat{W}_i x,\, (1-\widehat{W}_i) y \bigr]\mbox{,}}
where \eqn{\widehat{p}_{3,i}} are the weights for the \emph{maximum Euclidean likelihood} estimator (see \code{\link{spectralmeas}}) and \eqn{\widehat{W}_i} are the \emph{pseudo-polar angles} (see \code{\link{spectralmeas}}) for the index set \eqn{I_n} defined by \eqn{I_n = \{i = 1, \ldots, n : \widehat{S}_i > \widehat{S}_{(k{+}1)}\}}, where \eqn{\widehat{S}_{(k+1)}} denotes the \eqn{(k{+}1)}-th largest observation of the pseudo-polar radii \eqn{\widehat{S}_i} where the cardinality of \eqn{I_n} is exactly \eqn{k} elements long. (Tentatively, then this definition of \eqn{I_n} is ever so slightly different than in \code{\link{spectralmeas}}.)  Lastly, see the multiplier of \eqn{2} on the smooth form, and this multiplier is missing in Kiriliouk \emph{et al.} (2016, p. 365) but shown in Kiriliouk \emph{et al.} (2016, eq. 17.14, p. 360). Numerical experiments indicate that the \eqn{2} is needed for \eqn{\widehat{l}_{CF}(x,y)} but evidently not in \eqn{\widehat{l}(x,y)}.

The visualization of \eqn{l(x,y)} commences by setting a constant (\eqn{c > 0}) as \eqn{c_i \in 0.2,0.4,0.6,0.8} (say). The \eqn{y} are solved for \eqn{x \in [0,\ldots,c_i]} through the \eqn{l(x,y)} for each of the \eqn{c_i}. Each solution set constitutes a \emph{level set} for the stable tail dependence function. If the bivariate data have \emph{asymptotic independence} (to the right), then a level set or the level sets for all the \eqn{c} are equal to the lines \eqn{x + y = c}. Conversely, if the bivariate data have \emph{asymptotic dependence} (to the right), then the level sets will make 90-degree bends for \eqn{\mathrm{max}(x,y) = c}.
}
\usage{
stabtaildepf(uv=NULL, xy=NULL, k=function(n) as.integer(0.5*n), levelset=TRUE,
             ploton=TRUE, title=TRUE, delu=0.01, smooth=FALSE, ...)
}
\arguments{
  \item{uv}{An \R \code{data.frame} of \eqn{u} and \eqn{v} nonexceedance probabilities in the respective \eqn{X} (horizontal) and \eqn{Y} (vertical) directions. Note, \code{rank()}s are called on these so strictly speaking this need not be as nonexceedance probabilities. This is not an optional argument;}
  \item{xy}{A vector of the scalar coordinates \eqn{(x,y)}, which are \dQuote{the relative distances to the upper endpoints of [these respective] variables} (Kiriliouk \emph{et al.}, 2016, p. 356). This is a major point of nomenclature confusion. If these are in probability units, they are \emph{exceedance probabilities}. Though tested for \code{NULL} and a warning issued, these can be \code{NULL} only if \code{levelset=TRUE} but can be set to \code{xy=NA} if \code{levelset=FALSE} and \code{smooth=TRUE} (see discussion in \bold{Note});}
  \item{k}{The \eqn{k} for both the \eqn{\widehat{l}(x,y)} and \eqn{\widehat{l}_{CF}}, though the effect of \eqn{k} might not quite be the same for each. The default seems to work fairly well;}
  \item{levelset}{A logical triggering the construction of the level sets for \eqn{c} \eqn{=} \code{seq(0.1,} \code{1,} \code{by=0.1)};}
  \item{ploton}{A logical to call the \code{plot()} function;}
  \item{title}{A logical to trigger a title for the plot if \code{ploton=TRUE};}
  \item{delu}{The \eqn{\Delta x} for a sequence of \eqn{x} \eqn{=} \code{seq(0,c,by=delu)};}
  \item{smooth}{A logical controlling whether \eqn{\widehat{l}(x,y)} or the \enc{Capéraà}{Caperaa}--\enc{Fougères}{Fougeres} smooth function \eqn{\widehat{l}_{CF}(x,y)} is used; and}
  \item{...}{Additional arguments to pass.}
}
\value{
Varies according to argument settings. In particular, the \code{levelset=TRUE} will cause an \R \code{list} to be returned with the elements having the character string of the respective \eqn{c} values and the each holding a \code{data.frame} of the \eqn{(x,y)} coordinates.
}
\note{
This function is also called in a secondary recursion mode. The default \code{levelset=TRUE} makes a secondary call with \code{levelset=FALSE} to compute the \eqn{\widehat{l}(x,y)} for the benefit of the looping on the one-dimensional root to solve for a single \eqn{y} in \eqn{\widehat{l}(x,y) = c} given a single \eqn{x}. If \code{levelset=TRUE} and \code{smooth=TRUE}, then a secondary call with \code{smooth=TRUE} and \code{levelset=FALSE} is made to internally return an \R \code{list} containing scalar \eqn{N_n} and vectors \eqn{\widehat{W}_n} and \eqn{\widehat{p}_{3,i}} for similar looping and one-dimensional rooting for \eqn{\widehat{l}_{CF}(x,y)}.

If \code{levelset=FALSE}, then \code{xy} is required to hold the \eqn{(x,y)} coordinate pair of interest. A demonstration follows and shows the limiting behavior of a random sample from the \code{\link{N4212cop}} copula.
\preformatted{
  n <- 2000 # very CPU intensive this and the next code snippet
  UV <- simCOP(n=n, cop=N4212cop, para=pi); k <- 1:n
  lhat <- sapply(k, function(j)
                 stabtaildepf(xy=c(0.1, 0.1), uv=UV, levelset=FALSE, k=j))
  plot(k, lhat, xlab="k in [1,n]", cex=0.8, lwd=0.8, type="b",
                ylab="Empirical Stable Tail Dependence Function")
  mtext("Empirical function in the 0.10 x 0.10 Pr square (upper left corner)")
}

The \R \code{list} that can be used to compute \eqn{\widehat{l}_{CF}(x,y)} is retrievable by
\preformatted{
  x <- 0.1; y <- 0.1; k <- 1:(n-1)
  lhatCF <- sapply(k, function(j) {
     Hlis <- stabtaildepf(xy=NA, uv=UV, levelset=FALSE, smooth=TRUE, k=j)
     2*sum(Hlis$p3 * sapply(1:Hlis$Nn, function(i) {
                 max(c(Hlis$Wn[i]*x, (1-Hlis$Wn[i])*y)) }))
  })
  lines(k, lhatCF, col="red")
}
The smooth line (red) of \code{lhatCF} is somewhat closer to the limiting behavior of \code{lhat}, but it is problematic to determine computational consistency. Mathematical consistency with Kiriliouk \emph{et al.} (2016) appears to be achieved. The \bold{Examples} section TODO.
}
\references{
Beirlant, J., Escobar-Bach, M., Goegebeur, Y., Guillou, A., 2016, Bias-corrected estimation of stable tail dependence function: Journal Multivariate Analysis, v. 143, pp. 453--466, \doi{10.1016/j.jmva.2015.10.006}.

Kiriliouk, Anna, Segers, Johan, \enc{Warchoł}{Warchol}, \enc{Michał}{Michal}, 2016, Nonparameteric estimation of extremal dependence: \emph{in} Extreme Value Modeling and Risk Analysis, D.K. Dey and Jun Yan \emph{eds.}, Boca Raton, FL, CRC Press, ISBN 978--1--4987--0129--7.
}
\author{William Asquith \email{william.asquith@ttu.edu}}
\seealso{\code{\link{psepolar}}, \code{\link{spectralmeas}}
}
\examples{
\dontrun{
UV <- simCOP(n=1200, cop=GLcop, para=2.1) # Galambos copula
tmp1 <- stabtaildepf(UV) # the lines are curves (strong tail dependence)
tmp2 <- stabtaildepf(UV, smooth=TRUE, ploton=FALSE, col="red") #}
}
\keyword{stable tail dependence function}
\keyword{tail dependence}
\keyword{literature errors and inconsistencies}
