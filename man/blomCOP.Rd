\encoding{utf8}
\name{blomCOP}
\alias{blomCOP}
\title{The Blomqvist Beta of a Copula}
\description{
Compute the \emph{Blomqvist Beta} \eqn{\beta_\mathbf{C}} of a copula (Nelsen, 2006, p. 182), which is defined at the middle or center of \eqn{\mathcal{I}^2} as

\deqn{\beta_\mathbf{C} = 4\times\mathbf{C}\biggl(\frac{1}{2},\frac{1}{2}\biggr) - 1\mbox{,}}

where the \eqn{u = v = 1/2} and thus shows that \eqn{\beta_\mathbf{C}} is based on the \emph{median joint probability}. The Blomqvist Beta is also called the \emph{medial correlation coefficient}. Nelsen also reports that \dQuote{although, the Blomqvist Beta depends only on the copula only through its value at the center of \eqn{\mathcal{I}^2}, but that [\eqn{\beta_\mathbf{C}}] nevertheless often provides an accurate approximation to both \emph{Spearman Rho} \code{\link{rhoCOP}} and \emph{Kendall Tau} \code{\link{tauCOP}}.}  Kendall Tau \eqn{\tau_\mathbf{C}}, \emph{Gini Gamma} \eqn{\gamma_\mathbf{C}}, and Spearman Rho \eqn{\rho_\mathbf{C}} in relation to \eqn{\beta_\mathbf{C}} satisfy the following inequalities (Nelsen, 2006, exer. 5.17, p. 185):
\deqn{\frac{1}{4}(1 + \beta_\mathbf{C})^2  - 1 \le \tau_\mathbf{C} \le 1 - \frac{1}{4}(1 - \beta_\mathbf{C})^2\mbox{,}}
\deqn{\frac{3}{16}(1 + \beta_\mathbf{C})^3  - 1 \le \rho_\mathbf{C} \le 1 - \frac{3}{16}(1 - \beta_\mathbf{C})^3\mbox{, and}}
\deqn{\frac{3}{8}(1 + \beta_\mathbf{C})^2  - 1 \le \tau_\mathbf{C} \le 1 - \frac{3}{8}(1 - \beta_\mathbf{C})^2\mbox{.}}

A curious aside (Joe, 2014, p. 164) about the \emph{Gaussian copula} is that \emph{Blomqvist Beta} is equal to \emph{Kendall Tau} (\code{\link{tauCOP}}): \eqn{\beta_\mathbf{C} = \tau_\mathbf{C}} (see \bold{Note} in \code{\link{med.regressCOP}} for a demonstration). Finally, a version of Blomqvist Beta defined outside the median is provided by \code{\link{blomCOPss}}.
}
\usage{
blomCOP(cop=NULL, para=NULL, as.sample=FALSE,
                  ctype=c("joe", "weibull", "hazen", "1/n",
                                 "bernstein", "checkerboard"), ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat\beta_\mathbf{C}} (see \bold{Note});}
  \item{ctype}{Argument of the same as \code{\link{EMPIRcop}} with the exception of the \code{"joe"} specific to the documentation here. The other choices trigger and are given over to the empirical copula; and}
  \item{...}{Additional arguments to pass to the copula or down to \code{\link{EMPIRcop}} if a sample version had been requested.}
}
\value{
  The value for \eqn{\beta_\mathbf{C}} or sample \eqn{\hat\beta_n} is returned.
}
\note{
The sample \eqn{\hat\beta_n} is most efficiently computed (Joe, 2014, p. 57) by
\deqn{\hat\beta_n = \frac{2}{n} \sum_{i=1}^{n} \mathbf{1}\biggl([r_{i1} - (1 + n)/2]\times
                                                          [r_{i2} - (1 + n)/2] \ge 0\biggr) -
                    1\mbox{,}}
where \eqn{r_{i1}, r_{i2}} are the ranks of the data for \eqn{i = 1, \ldots n}, and \eqn{\mathbf{1}(.)} is an \emph{indicator function} scoring 1 if condition is true otherwise zero. However, the Joe sample estimator is not fully consistent (or vice versa) with the various versions of the empirical copula, \eqn{\mathbf{C}_n}, (\code{\link{EMPIRcop}}) (see the last example in \bold{Examples}). Also, the nature of even and odd sample sizes controls how the median is computed and the issue of samples lying on the median lines in \eqn{U} and \eqn{V} (Genest \emph{et al.}, 2013). The argument \code{ctype} supports triggers to the \eqn{\mathbf{C}_n} in lieu of the Joe sample estimator shown in this documentation.
}
\references{
Genest, C., Carabar\enc{í}{i}n-Aguirre, A., and Harvey, F., 2013, Copula parameter estimation using\cr Blomqvist's beta: Journal de la Soci\eqn{é}{e}t\enc{é}{e} Fran\enc{ç}{c}aise de Statistique, v. 154, no. 1, pp. 5--24.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{blomCOPss}}, \code{\link{blomatrixCOPdec}}, \code{\link{blomatrixCOPiqr}},
         \code{\link{footCOP}}, \code{\link{giniCOP}}, \code{\link{hoefCOP}},
         \code{\link{rhoCOP}}, \code{\link{tauCOP}}, \code{\link{wolfCOP}},
         \code{\link{joeskewCOP}}, \code{\link{uvlmoms}}
}
\examples{
blomCOP(cop=PSP) # 1/3 precisely

\dontrun{
# Nelsen (2006, exer. 5.17, p. 185) : All if(...) are TRUE
B <- blomCOP(cop=N4212cop, para=2.2); Bp1 <- 1 + B; Bm1 <- 1 - B
G <- giniCOP(cop=N4212cop, para=2.2); a <- 1/4; b <- 3/16; c <- 3/8
R <-  rhoCOP(cop=N4212cop, para=2.2)
K <-  tauCOP(cop=N4212cop, para=2.2, brute=TRUE) # numerical issues without brute
if( a*Bp1^2 - 1 <= K & K <= 1 - a*Bm1^2 ) print("TRUE") #
if( b*Bp1^3 - 1 <= R & R <= 1 - b*Bm1^3 ) print("TRUE") #
if( c*Bp1^2 - 1 <= G & G <= 1 - c*Bm1^2 ) print("TRUE") #}

\dontrun{
# A demonstration of a special feature of blomCOP for sample data.
# Joe (2014, p. 60; table 60) has 0.749 for GHcop(tau=0.5); n*var(hatB) as n-->infinity
set.seed(1)
theta <- GHcop(tau=0.5)$para; B <- blomCOP(cop=GHcop, para=theta); n <- 1000
H <- sapply(1:1000, function(i) { # Let us test that with pretty large sample size:
	                blomCOP(para=rCOP(n=n, cop=GHcop, para=theta), as.sample=TRUE) })
print(n*var(B-H)) # For 1,000 sims of size n : 0.789, nearly matches Joe's result }

\dontrun{
# Joe (2014, p. 57) says that sqrt(n)(B-HatBeta) is Norm(0, 1 - B^2)
set.seed(1)
n <- 10000; B <- blomCOP(cop=PSP) # Beta = 1/3
H <- sapply(1:100, function(i) { message(i,"-", appendLF=FALSE)
	               blomCOP(para=rCOP(n=n, cop=PSP), as.sample=TRUE) })
lmomco::parnor(lmomco::lmoms(sqrt(n)*(H-B))) # mu = -0.038; sigma = 0.970
# Joe (2014) : sqrt(1-B^2) == standard deviation (sigma) : (1-(1/3)^2) approx 0.973 }

\dontrun{
nn <- 200; set.seed(1)
UV <- simCOP(n=nn+1, cop=PSP, graphics=FALSE)
for(n in nn:(nn+1)) {
  if(as.logical(n \%\% 2)) { # in source \ percent \ percent for latex
    message("Blomquist Betas for an odd  sample size n=", n)
    uv <- UV
  } else {
    message("Blomquist Betas for an even sample size n=", n)
    uv <- UV[-(nn+1), ] # remove the last and 'odd' indexed value to make even
  }
  message(c(" Joe2014: ", blomCOP(as.sample=TRUE, para=uv, ctype="joe"         )))
  message(c(" Weibull: ", blomCOP(as.sample=TRUE, para=uv, ctype="weibull"     )))
  message(c(" Hazen:   ", blomCOP(as.sample=TRUE, para=uv, ctype="hazen"       )))
  message(c(" 1/n:     ", blomCOP(as.sample=TRUE, para=uv, ctype="1/n"         )))
  message(c(" Bernstn: ", blomCOP(as.sample=TRUE, para=uv, ctype="bernstein"   )))
  message(c(" ChckBrd: ", blomCOP(as.sample=TRUE, para=uv, ctype="checkerboard")))
}
# Blomquist Betas for an even sample size n=200
#  Joe2014: 0.32
#  Weibull: 0.32
#  Hazen:   0.32
#  1/n:     0.32
#  Bernstn: 0.323671819423416
#  ChckBrd: 0.32
# Blomquist Betas for an odd  sample size n=201
#  Joe2014: 0.323383084577114
#  Weibull: 0.333333333333333
#  Hazen:   0.333333333333333
#  1/n:     0.293532338308458
#  Bernstn: 0.327747577290946
#  ChckBrd: 0.313432835820896 #}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{medial correlation coefficient}
\concept{medial correlation}
\keyword{Joe (2014) Examples and Exercises}
\keyword{Nelsen (2006) Examples and Exercises}
\keyword{Gaussian copula}
\keyword{Normal copula}
\keyword{Blomqvist Beta}
