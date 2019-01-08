\encoding{utf8}
\name{duCOP}
\alias{duCOP}
\title{ The Dual of a Copula Function }
\description{
Compute the \emph{dual of a copula (function)} from a copula (Nelsen, 2006, pp. 33--34), which is defined as

\deqn{\mathrm{Pr}[U \le v \mathrm{\ or\ } V \le v] = \tilde{\mathbf{C}}(u,v) = u + v - \mathbf{C}(u,v)\mbox{,}}

where \eqn{\tilde{\mathbf{C}}(u,v)} is the dual of a copula and \eqn{u} and \eqn{v} are nonexceedance probabilities. The dual of a copula is the expression for the probability that either \eqn{U \le u} \bold{or} \eqn{V \le v}, which is unlike the \emph{co-copula (function)} (see \code{\link{coCOP}}) that provides \eqn{\mathrm{Pr}[U > u \mathrm{\ or\ } V > v]}. The dual of a copula is a function and not in itself a copula. The dual of the  \emph{survivial copula} (\code{\link{surCOP}}) is the \emph{co-copula (function)} (\code{\link{coCOP}}). Some rules of copulas mean that

\deqn{\hat\mathbf{C}(u',v') + \tilde{\mathbf{C}}(u,v) \equiv 1\mbox{,}}

where \eqn{\hat\mathbf{C}(u',v')} is the survival copula in terms of exceedance probabilities \eqn{u'} and \eqn{v'} or in \pkg{copBasic} code that the functions \code{\link{surCOP}} + \code{duCOP} equal unity.

The function \code{duCOP} gives \dQuote{protection} against simultaneous (concurrent or dual) risk by failure if and only if failure is caused (defined) by both hazard sources \eqn{U} and \eqn{V} occurring at the same time. Expressing this in terms of an annual probability of occurrence (\eqn{q}), one has
\deqn{q = 1 - \mathrm{Pr}[U \le v \mathrm{\ or\ } V \le v] = 1 - \tilde{\mathbf{C}}(u,v)\mbox{\ or}}
in \R code \code{q <- 1 - duCOP(u,v)}. So as a mnemonic: \emph{A dual of a copula is the probabililty of nonexceedance if the hazard sources must \bold{dual} (concur, link, pair, twin, twain) to cause failure.} An informative graphic is shown within \code{\link{copBasic-package}}.
}
\usage{
duCOP(u, v, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass (such as parameters, if needed, for the copula in the form of a list.}
}
\value{
  Value(s) for the dual of a copula are returned.
}
\note{
There can be confusion in the interpretation and implemenation of the \bold{or} condition of \emph{joint probability} provided by \eqn{\tilde{\mathbf{C}}(u,v)}. Two types of \bold{or}'s seemingly exist depending on one's concept of the meaning of \dQuote{or.}  To start, there is the \dQuote{either or both} conceptualization (\bold{join or}) that encompasses either \dQuote{event} (say a loss) of importance for random variables \eqn{U} and \eqn{V} \emph{as well as} the \bold{joint and} conditions where both variables simultaneously are generating an event of importance.

Let us continue by performing a massive simulation for the \eqn{\mathbf{PSP}(u,v)} copula (\code{\link{PSP}}) and set an either event standard on the margins as 10 percent for an arbitrary starting point. The \eqn{\mathbf{PSP}} has positive association with lower tail dependency, and the example here considers the left tail as the risk tail.
\preformatted{
  Event <- 0.1; nn <- 100000; set.seed(9238)
  UV <- simCOP(n=nn, cop=PSP, graphics=FALSE) # 1E5 realizations
}
Next, let us step through counting and then make theoretical comparisons using copula theory. The \bold{joint and} condition as nonexceedances is
\preformatted{
  ANDs <- length(UV$U[UV$U <= Event & UV$V <= Event]) / nn
  ANDt <- COP(Event, Event, cop=PSP)
  message(   "Joint AND by simulation = ", round(ANDs, digits=5),
           "\n    Joint AND by theory = ", round(ANDt, digits=5))
  # ANDs = 0.05348 and ANDt = 0.05263 (numerical congruence)
}
where it is obvious that the simulations and theory estimate about the same \bold{joint and} condition. Now, the \bold{joint or} condition as nonexceedances is
\preformatted{
  ORs <- length(UV$U[UV$U <= Event | UV$V <= Event]) / nn
  ORt <- duCOP(Event, Event, cop=PSP)
  message(   "Joint OR by simulation = ", round(ORs, digits=5),
           "\n    Joint OR by theory = ", round(ORt, digits=5))
  # ORs = 0.14779 and ORt = 0.14737 (numerical congruence)
}
where it is obvious that the simulations and theory estimate about the same \bold{joint or} condition. Finally, the joint \bold{mutually exclusive or} condition as nonexceedances is
\preformatted{
  eORs <- length((UV$U[(UV$U <= Event | UV$V <= Event) &
                     ! (UV$U <= Event & UV$V <= Event)])) / nn
  eORt <- ORt - ANDt # theoretical computation
  message(   "Joint exclusive OR by simulation = ", round(eORs, digits=5),
           "\n    Joint exclusive OR by theory = ", round(eORt, digits=5))
  # eORs = 0.09431 and eORt = 0.09474 (numerical congruence)
}
where it is obvious that the simulations and theory estimate about the same joint \bold{mutually exclusive or} condition, and where it is shown that the prior two theoretical joint probabilities can be subtracted from each to yield the \bold{mutually exclusive or} condition.


Let us then play out a scenario in which it is judged that of the events causing damage that the simultaneous occurrance is worse but that engineering against about 5 percent of events not occurring at the same time represents the most funding available. Using numerical methods, it is possible to combine \eqn{\tilde\mathbf{C}} and \eqn{\mathbf{C}} and assume equal marginal risk in \eqn{U} and \eqn{V} as the following list shows:
\preformatted{
  "designf" <- function(t) { # a one-off function just for this example
     duCOP(t, t, cop=PSP) - COP(t, t, cop=PSP) - 5/100 # 5 percent
  }
  dThres <- uniroot(designf, c(.Machine$double.eps,0.5))$root
}
where the \code{uniroot} function performs the optimization and the \code{.Machine$double.eps} value is used because the \eqn{\mathbf{PSP}} is \code{NaN} for zero probability. (It is unity for unity marginal probabilities.)

The design threshold on the margins then is \code{dThres} \eqn{\approx} 0.05135. In other words, the \code{designThres} is the marginal probability that results in about 5 percent of events not occurring at the same time. Then considering the simulated sample and counting the nonexceedances by code one achieves:
\preformatted{
  Damage       <- length( UV$U[ UV$U <= dThres | UV$V <= dThres ])
  SimDamage    <- length( UV$U[ UV$U <= dThres & UV$V <= dThres ])
  NonSimDamage <- length((UV$U[(UV$U <= dThres | UV$V <= dThres) &
                             ! (UV$U <= dThres & UV$V <= dThres)]) )
  message(  "                 Damaging Events (sim.) = ", Damage,
          "\n    Simultaneous damaging events (sim.) = ", SimDamage,
          "\n Nonsimultaneous damaging events (sim.) = ", NonSimDamage)
}
but also the theoretical expectations are readily computed using copula theory:
\preformatted{
  tDamage       <- as.integer(duCOP(dThres, dThres, cop=PSP) * nn)
  tSimDamage    <- as.integer(  COP(dThres, dThres, cop=PSP) * nn)
  tNonSimDamage <- tDamage - tSimDamage
  message(  "                 Damaging Events (theory) = ", tDamage,
          "\n    Simultaneous damaging events (theory) = ", tSimDamage,
          "\n Nonsimultaneous damaging events (theory) = ", tNonSimDamage)
}
The counts from the former listing are 7,670; 2,669; and 5,001, whereas the respective counts from the later listing are 7,635; 2,635; and 5,000. Numerical congruency in the counts thus exists.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{coCOP}}, \code{\link{surCOP}}, \code{\link{jointCOP}}, \code{\link{joint.curvesCOP}} }
\examples{
u <- runif(1); t <- runif(1)
duCOP(cop=W,u,t)    # joint or probability for perfect negative dependence
duCOP(cop=P,u,t)    # joint or probability for perfect        independence
duCOP(cop=M,u,t)    # joint or probability for perfect positive dependence
duCOP(cop=PSP,u,t)  # joint or probability for some positive    dependence

# Next demonstrate COP + duCOP = unity.
"MOcop.formula" <- function(u,v, para=para, ...) {
   alpha <- para[1]; beta <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
}
"MOcop" <- function(u,v, ...) { asCOP(u,v, f=MOcop.formula, ...) }

u <- 0.2; v <- 0.75; ab <- c(1.5, 0.3)
surCOP(1-u,1-v, cop=MOcop, para=ab) + duCOP(u,v, cop=MOcop, para=ab) # UNITY

# See extended code listings and discussion in the Note section}
\keyword{copula operator}
\keyword{joint probability}
\keyword{joint probability (exclusive or)}
\concept{exclusive or}
\concept{mutually exclusive or}
\concept{mutually exclusive or condition}
