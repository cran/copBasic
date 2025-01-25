\encoding{utf8}
\name{jointCOP}
\alias{jointCOP}
\title{Compute Equal Marginal Probabilities Given a Single Joint AND or OR Probability for a Copula}
\description{
Given a single \emph{joint probability} denoted as \eqn{t} for a copula \eqn{\mathbf{C}(u,v)} numerically solve for bivariate marginal probabilities \eqn{U} and \eqn{V} such that they are also equal to each other (\eqn{u = v = w}). For the case of a \bold{joint and} probability, the primary diagonal of the copula (Nelsen, 2006, pp. 12 and 16) is solved for by a simple dispatch to the \code{\link{diagCOPatf}} function instead. Symbolically the solution is
\deqn{\mathrm{Pr}[U \le v,\ V \le v] = t = \mathbf{C}(w,w)\mbox{.}}


For the case of a \bold{joint or} probability, the \emph{dual of a copula (function)} or \eqn{\tilde{\mathbf{C}}(u,v)} from a copula (Nelsen, 2006, pp. 33--34; \code{\link{duCOP}}) is used where symbolicaly the solution is
\deqn{\mathrm{Pr}[U \le v \mathrm{\ or\ } V \le v] = t = \tilde{\mathbf{C}}(u,v) = u + v - \mathbf{C}(u,v)\mbox{,}}
or
\deqn{\mathrm{Pr}[U \le v \mathrm{\ or\ } V \le v] = t = 2w - \mathbf{C}(w,w)\mbox{.}}

The function for \code{type="or"} tests \eqn{\tilde{\mathbf{C}}(0,0)} and if it returns \code{NA} or \code{NaN} then the lower limit for the rooting is treated as \code{.Machine$double.eps} instead of 0 (zero).
}
\usage{
jointCOP(t, cop=NULL, para=NULL, type=c("and", "or"), ...)
}
\arguments{
  \item{t}{The joint probability level \eqn{t};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{type}{The type of joint probability is to be computed; and}
  \item{...}{Additional arguments to pass to the \code{\link{duCOP}} function of \pkg{copBasic} or \code{uniroot()} function.}
}
\value{
  A vector of the equal \eqn{u} and \eqn{v} probabilties for the given \code{type} at the joint probability level of \eqn{t}. The vector includes the \eqn{t} as the third element.
}
\note{
\emph{ENSEMBLE 1---Counting and Copula Probabilities from a Massive Sample Size:} Simulations can be used to check and verify select copula concepts. Begin with a \emph{Gumbel--Hougaard} copula \eqn{\mathbf{GH}(u,v) = \mathbf{C}_{\Theta}(u,v)} having parameter \eqn{\Theta = 1.5}, which corresponds to a \emph{Kendall Tau} \eqn{\tau_\mathbf{C} = 1/3} (\code{\link{GHcop}}). Next, simulate and count the number of either \eqn{U} or \eqn{V} exceeding the 99th percentile \dQuote{event.} The event can occur either from the random variable \eqn{U} or from \eqn{V} with equal \dQuote{loss.} If the event occurs in both \eqn{U} and \eqn{V}, the loss is just the same as if \eqn{U} or \eqn{V} occurred. So, if a design were at the 100-year level and thus a 0.01 chance of loss each year, then 500 losses in 50,000 years would be expected.
\preformatted{
  set.seed(89); n <- 50000
  UV <- simCOP(n, cop=GHcop, para=1.5, graphics=FALSE)
  length(UV$U[UV$U > 0.99    | UV$V > 0.99])     # 799 times (losses)
  length(UV$U[UV$U > 0.99356 | UV$V > 0.99356])  # 500 times (losses)
}
Letting \code{JP} equal 0.99356, which forces the required acceptance of 500 losses for the design, has conditions of \code{UV$U > JP} \bold{or} \code{UV$V > JP} as well as condition of \code{UV$U > JP} \bold{and} \code{UV$V > JP}. These three conditions are captured using the structure of the \R code listed. Manual searching resulted in a value for \code{JP} equaling 0.99356, which produces the 500 count losses (acceptable losses). Thus, \code{JP} is a marginal bivariate probability (in this case equality between \eqn{U_{\mathrm{crit.}} = V_{\mathrm{crit.}}} declared) necessary to attain a 99th percentile joint protection from loss. The magnitude for either \eqn{U} or \eqn{V} thus must exceed the 99th percentile, and this is what the code shows with 799 losses.

It is important to consider that unless \eqn{U} and \eqn{V} are in perfect positive correlation (\emph{e.g.} \eqn{\mathbf{M}(u,v)}, \code{\link{M}}, \emph{\enc{Fréchet}{Frechet}--Hoeffding upper-bound copula}), that protection from loss needs to be higher than 0.99 if the marginal risk is set at that level. Continuing, if the 99th percentile is the 100-year event, then design criteria should be about 155 years instead [\code{lmomco::prob2T(0.99356)}]. The user can readily see this with the switch to perfect independence with the \eqn{\mathbf{GH}(u,v)} copula with \eqn{\Theta = 1} and produce quite different results or extreme correlation with say \eqn{\Theta > 20}.

To provide the protection for 500 exceedances in \eqn{n =} 50,000 trials and for purposes of demonstration, balance the protection between \eqn{U} and \eqn{V} by setting their probabilities equal, then the theoretical joint probability is
\preformatted{
  diagCOPatf(0.99, 0.99, cop=GHcop, para=1.5)             # 0.9936887
  jointCOP(  0.99,       cop=GHcop, para=1.5, type="and") # 0.9936887
}
and these two probabilities, which in reality are actually based on same computation (\code{\link{diagCOPatf}}), and nearly are the same as \code{JP} \eqn{=} 0.99356 that was determined by the manual searching on the simulated data.

A \bold{mutually inclusive and} condition can be arranged as follows, and it is implicit in the definition that both loss events occur at the same time:
\preformatted{
  length(UV$U[UV$U > 0.99 & UV$V > 0.99])            # 208 losses ( simulated )
  surCOP(  1-0.99, 1-0.99, cop=GHcop, para=1.5) * n  # 209 losses (theoretical)
  surfuncCOP(0.99,   0.99, cop=GHcop, para=1.5) * n  # 209 losses (theoretical)
}

What are the expected number of exceedances if designs for \eqn{U} and \eqn{V} are built at \eqn{U =} \eqn{V = 0.99} marginal probabilities for protection?
\preformatted{
   coCOP(1-0.99, 1-0.99, cop=GHcop, para=1.5)  * n  # 791 losses (theoretical)
  # by the co-copula which from the copula as nonexceedances is
  (1-COP(  0.99,   0.99, cop=GHcop, para=1.5)) * n  # 791 losses (theoretical)
}
Note, the 791 losses is nearly equal to 799, but obviously not 500 as one might incorrectly have imagined strictly in a univariate world.

Now, a couple of questions can be asked about the \bold{joint and} and \bold{joint or} probabilities using the definition of a copula \code{\link{COP}} and the \emph{dual of a copula (function)} (\eqn{\tilde{\mathbf{C}}(u,v)}, \code{\link{duCOP}}), respectively:
\preformatted{
  # The AND nonexceedances:
  length(UV$U[UV$U <= 0.99 & UV$V <= 0.99]) / n    # 0.98402   ( simulated )
    COP(0.99, 0.99, cop=GHcop, para=1.5)           # 0.9841727 (theoretical)

  # The OR nonexceedances:
  length(UV$U[UV$U <= 0.99 | UV$V <= 0.99]) / n    # 0.99584   ( simulated )
  duCOP(0.99, 0.99, cop=GHcop, para=1.5)           # 0.9958273 (theoretical)
}

How about inversion of \eqn{\tilde{\mathbf{C}}(u,v)} by \code{jointCOP} and check against the simulation?
\preformatted{
  jointCOP(0.99, cop=GHcop, para=1.5, type="or")[1]      # 0.9763951 ( theor. )
  length(UV$U[UV$U <= 0.9763951 | UV$V <= 0.9763951])/n  # 0.98982 ( simulated)
}
The second probability is a value quite near to 0.99. So, if one wants mutual loss protection, compute the inversion of the dual of a copula using \code{jointCOP(..., type="or")}. Let us say that 0.80 mutual loss protection is wanted
\preformatted{
  jointCOP(0.80, cop=GHcop, para=1.5, type="or")[1]      # 0.6561286
  n - length(UV$U[UV$U <= 0.6561286 | UV$V <= 0.6561286])# 10049 losses ( sim.)
  n - (1-0.8)*n                                    # 10000 losses (theoretical)
}

The example here shows numerical congruence of 10,049 \eqn{\approx} 10,000. An opposing question is useful. How about a \bold{mutually exclusive or} condition as nonexceedances?
\preformatted{
  # The mutually exclusive OR as nonexceedances:
  length((UV$U[  (UV$U <= 0.99 | UV$V <= 0.99) &
               ! (UV$U <= 0.99 & UV$V <= 0.99)]))        # 591 losses ( simulated )
  # The mutually exclusive OR as exceedances:
  length(UV$U[   (UV$U >  0.99 | UV$V >  0.99) &
               ! (UV$U >  0.99 & UV$V >  0.99)])         # 591 losses ( simulated )
}
It is clear that 208 \eqn{+} 591 \eqn{=} 799 as shown earlier; notice how there are two ways to get at the 591 count. There are 208 mutual loss events and 591 occassions where either \eqn{U} or \eqn{V} is the causation of loss. For comparison, how many observed events by random variable were observed?
\preformatted{
  length(UV$U[  (UV$U > 0.99)]) # 519 ( simulated )
  length(UV$U[  (UV$V > 0.99)]) # 491 ( simulated )
}
If the variables were perfectly uncorrelated (\eqn{\mathbf{P}(u,v)}, \code{\link{P}}, \emph{independence copula}) would be 519 \eqn{+} 491 \eqn{=} 1,007 losses. But for the simulations here, 799 losses occurred, so 1,007 \eqn{-} 799 \eqn{=} 208 losses were at the same time caused by \eqn{U} and \eqn{V}.

Both of the following lengths \code{A} and \code{B} are 799 and both represent a \bold{joint or} condition---the operations do not represent a \bold{mutually exclusive or} condition.
\preformatted{
  A <- length(UV$U[UV$U > 0.99]) + length(UV$U[UV$V > 0.99]) -
       length(UV$U[UV$U > 0.99 & UV$V > 0.99])
  B <- length(UV$U[UV$U > 0.99 | UV$V > 0.99]) # A == B == 799
}

\emph{ENSEMBLE 2---Simulation Study using a Real-World Sample Size:} Now with identities of sorts shown and described above, let us test a theoretically consistent version of a sample of size 150 repeated 1,000 times at the 98th percentile against loss by one or the other random variables or both for slightly correlated \eqn{U} or \eqn{V} again following the Gumbel--Hougaard copula. The losses incurred by mutual event occurrence is the same as if one or the other variables produced an event causing loss.
\preformatted{
  n <- 250; nsim <- 1000; EitherEvent <- 0.98; MutualEvent <- 0.99; Theta<- 1.5
  PT <- jointCOP(EitherEvent, cop=GHcop, para=Theta, type="and")[1] # 0.9873537
  DU <- jointCOP(MutualEvent, cop=GHcop, para=Theta, type="or" )[1] # 0.9763951
}

This next code listing is a redundant example to the one that follows, but it is shown because a slight possibility of confusion in the vectorized conditional evaluations in \R. This first example concretely changes the loss events into binary states and adds them up prior to the condition.
\preformatted{
  set.seed(894234)
  EX1a <- sapply(1:nsim, function(i) {
                 uv <- simCOP(n, cop=GHcop, para=Theta, graphics=FALSE)
          length(uv$U[ as.numeric(uv$U > PT) + as.numeric(uv$V > PT) >= 1 ]) })
  t.test(EX1a, mu=(1-EitherEvent) * n)
}
The expected count \eqn{E[}\code{EX1a}\eqn{] =} 5 and the simulation run yielded 5.058. The \code{t.test()} function in \R results in a statistically insignificant difference. The following two example use a similar there. For sake of both code brevity and clarity, the examples here all restart the simulations at the expense of computation time.
\preformatted{
  set.seed(894234)
  EX1b <- sapply(1:nsim, function(i) {
                uv  <- simCOP(n, cop=GHcop, para=Theta, graphics=FALSE)
                length(uv$U[uv$U > PT | uv$V > PT])  })
  t.test(EX1b, mu=(1-EitherEvent) * n)
}
The expected count \eqn{E[}\code{EX1b}\eqn{] =} 5---the same results are shown as in the first example listing.

Next, let us demonstrate the dual of a copula for mutually occurring events.
\preformatted{
  set.seed(894234)
  EX2 <- sapply(1:nsim, function(i) {
                uv  <- simCOP(n, cop=GHcop, para=Theta, graphics=FALSE)
                length(uv$U[uv$U > DU & uv$V > DU]) })
  t.test(EX2, mu=(1-MutualEvent) * n)
}
The expected count \eqn{E[}\code{EX2}\eqn{] =} 2.5 and the simulation run yielded 2.49. The \code{t.test()} again results in a statistically insignificant difference.


The \eqn{U = V = 0.9873537} for the \dQuote{and} and \eqn{U = V = 0.9763951} for the \dQuote{or} are not equal marginal probabilities. Taking the larger of the two marginal probabilities, the actual joint protection from mutual event occurrance can be computed:
\preformatted{
  duCOP(0.9873537, 0.9873537, cop=GHcop, para=Theta) # 0.9947075
  (1-0.9947075)*n  # which is about 1.32 events per 250 trials.
}
So, the larger protection in terms of joint probabilities provided by \code{EitherEvent} at 0.98 instead of \code{MutualEvent} at 0.99 with respective protection at the 0.9873537 provides a \code{MutualEvent} protection of 0.9947075.
\preformatted{
  set.seed(894234)
  EX3 <- sapply(1:nsim, function(i) {
                uv  <- simCOP(n, cop=GHcop, para=Theta, graphics=FALSE)
                length(uv$U[uv$U > 0.9873537 & uv$V > 0.9873537]) })
  t.test(EX3, mu=(1-0.9947075) * n)
}
The expected count \eqn{E[}\code{EX3}\eqn{] =} 1.32 and the simulation run yielded 1.31. The \code{t.test()} again results in a statistically insignificant difference, and thus the result indistinguishable from the expectation.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{diagCOPatf}}, \code{\link{duCOP}}, \code{\link{joint.curvesCOP}}, \code{\link{level.curvesCOP}}
}
\examples{
jointCOP(0.50, cop=GHcop, para=1.5, type="and") # 0.6461941  0.6461941  0.5000000
jointCOP(2/3,  cop=GHcop, para=1.5, type="or" ) # 0.4994036  0.4994036  0.6666667

# See extended code listings and discussion in the Note section
}
\keyword{joint probability}
