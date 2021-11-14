\encoding{utf8}
\name{rhoCOP}
\alias{rhoCOP}
\title{ The Spearman Rho of a Copula }
\description{
Compute the measure of association known as the \emph{Spearman Rho} \eqn{\rho_\mathbf{C}} of a copula according to Nelsen (2006, pp. 167--170, 189, 208) by
\deqn{\rho_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} \mathbf{C}(u,v)\, \mathrm{d}u\mathrm{d}v - 3\mbox{,}}
or
\deqn{\rho_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} [\mathbf{C}(u,v) - uv]\, \mathrm{d}u\mathrm{d}v\mbox{,}}
where the later equation is implemented by \code{rhoCOP} as the default method (\code{method="default"}). This equation, here having \eqn{p = 1} and \eqn{k_p(1) = 12}, is generalized under \code{\link{hoefCOP}}. The absence of the \eqn{12} in the above equation makes it equal to the covariance defined by the \emph{Hoeffding identity} (Joe, 2014, p. 54):
\deqn{\mathrm{cov}(U, V) = \int\!\!\int_{\mathcal{I}^2} [\mathbf{C}(u,v) - uv]\, \mathrm{d}u\mathrm{d}v\mbox{ or}}
\deqn{\mathrm{cov}(U, V) = \int\!\!\int_{\mathcal{I}^2} [\hat\mathbf{C}(u,v) - uv]\, \mathrm{d}u\mathrm{d}v\mbox{, which is}}
\deqn{\mathrm{cov}(U, V) = \int\!\!\int_{\mathcal{I}^2} [u+v-1+\mathbf{C}(1-u,1-v) - uv]\, \mathrm{d}u\mathrm{d}v\mbox{.}}

Depending on copula family (Joe, 2014, pp. 56 and 267), the alternative formulation for \eqn{\rho_\mathbf{C}} could be used
\deqn{\rho_\mathbf{C} = 3 - 12\int\!\!\int_{\mathcal{I}^2} u \frac{\delta\mathbf{C}(u,v)}{\delta u} \, \mathrm{d}u\mathrm{d}v = 3 - 12\int\!\!\int_{\mathcal{I}^2} v\frac{\delta\mathbf{C}(u,v)}{\delta v} \, \mathrm{d}u\mathrm{d}v\mbox{,}}
where the first integral form corresponds to Joe (2014, eq. 248, p. 56) and is the \code{method="joe21"}, and the second integral form is the \code{method="joe12"}.

The integral
\deqn{\int\!\!\int_{\mathcal{I}^2} \mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v\mbox{,}}
represents the \dQuote{volume under the graph of the copula and over the unit square} (Nelsen, 2006, p. 170) and therefore \eqn{\rho_\mathbf{C}} is simple a rescaled volume under the copula. The second equation for \eqn{\rho_\mathbf{C}} expresses the \dQuote{average distance} between the joint distribution and statistical \emph{independence} \eqn{\mathbf{\Pi} = uv}. Nelsen (2006, pp. 175--176) shows that the following relation between \eqn{\rho_\mathbf{C}} and \eqn{\tau_\mathbf{C}} (\code{\link{tauCOP}}) exists
\deqn{-1 \le 3\tau - 2\rho \le 1\mbox{.}}
}
\usage{
rhoCOP(cop=NULL, para=NULL, method=c("default", "joe21", "joe12"),
                            as.sample=FALSE, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{method}{The form of integration used to compute (see above);}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat\rho} by dispatch to \code{cor()} function in \R with \code{method = "spearman"};}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force integration using \code{brute}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\rho_\mathbf{C}} is returned.
}
\note{
Technically, Nelsen (2006) also shows that these definitions are a form of call to a \emph{concordance function} \eqn{\mathcal{Q}(\mathbf{C}_1,\mathbf{C}_2)} of two copulas that involve \eqn{\mathbf{C}_1{=}\mathbf{C}(u,v)} and \eqn{\mathbf{C}_2{=}\mathbf{\Pi}}. As such in order to keep \code{rhoCOP} a small function when \code{brute=TRUE}, \eqn{\rho_\mathbf{C}} is computed by a special call to \code{\link{tauCOP}}, which by itself and although titled for computation of \emph{Kendall Tau}, does support the concordance function \eqn{\mathcal{Q}(\mathbf{C}_1, \mathbf{C}_2)} [see Nelsen (2006, pp. 158--159)] when given two different copulas and respective parameters as arguments.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}},
         \code{\link{hoefCOP}}, \code{\link{tauCOP}},  \code{\link{wolfCOP}},
         \code{\link{joeskewCOP}}, \code{\link{uvlmoms}}
}
\examples{
rhoCOP(cop=PSP)             # 0.4784176
\dontrun{
rhoCOP(cop=PSP, brute=TRUE) # 0.4684063
# CPU heavy example showing that the dual-integration (fast) results in
# a Spearman's Rho that mimics a sample version
do_rho <- function(n) {
   uv <- simCOP(n=n, cop=PSP, ploton=FALSE, points=FALSE)
   return(cor(uv$U, uv$V, method="spearman"))
}
rhos <- replicate(100, do_rho(1000))
rho_sample <- mean(rhos); print(rho_sample) # 0.472661}

\dontrun{
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.00395,    para2=4.67,     alpha=0.9392, beta=0.5699)
rhoCOP(cop=composite2COP, para=para) # -0.5924796

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.14147,    para2=20.96,    alpha=0.0411, beta=0.6873)
rhoCOP(cop=composite2COP, para=para) # 0.2818874

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.10137,     para2=4492.87, alpha=0.0063, beta=0.0167)
rhoCOP(cop=composite2COP, para=para)             # 0.9812919
rhoCOP(cop=composite2COP, para=para, brute=TRUE) # 0.9752155}

\dontrun{
# This is the same composited copula used in a highly asymmetric multi-modal
# plotting example under densityCOPplot(). Let us use that copula as a means to
# check on the Spearman Rho from the alternative formulations from Joe (2014).
para <- list(alpha=0.15, beta=0.90, kappa=0.06, gamma=0.96,
             cop1=GHcop, cop2=PLACKETTcop, para1=5.5, para2=0.07)
"rhoCOPbyJoe21" <- function(cop=NULL, para=NULL, ...) { # Joe (2014, eq. 2.48)
   myint <- NULL
   try(myint <- integrate(function(u) {
       sapply(u,function(u) { integrate(function(v) {
       u * derCOP( u, v, cop=cop, para=para, ...)},  0, 1)$value })}, 0, 1))
   ifelse(is.null(myint), return(NA), return(3 - 12*myint$value))
}
"rhoCOPbyJoe12" <- function(cop=NULL, para=NULL, ...) { # Not in Joe (2014)
   myint <- NULL
   try(myint <- integrate(function(u) {
       sapply(u,function(u) { integrate(function(v) {
       v * derCOP2( u, v, cop=cop, para=para, ...)}, 0, 1)$value })}, 0, 1))
   ifelse(is.null(myint), return(NA), return(3 - 12*myint$value))
}
rhoCOP(       cop=composite2COP, para=para) # 0.1031758
rhoCOPbyJoe21(cop=composite2COP, para=para) # 0.1031803
rhoCOPbyJoe12(cop=composite2COP, para=para) # 0.1031532}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{Joe (2014) Examples and Exercises}
\keyword{Spearman Rho}
