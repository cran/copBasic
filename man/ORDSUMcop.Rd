\encoding{utf8}
\name{ORDSUMcop}
\alias{ORDSUMcop}
\title{Ordinal Sums of M-Copula}
\description{
Compute \emph{ordinal sums of a copula} (Nelsen, 2006, p. 63) or \emph{M-ordinal sum of the summands} (Klement \emph{et al.}, 2017) within \eqn{\mathcal{I}^2} into \eqn{n} partitions (possibly infinite) within \eqn{\mathcal{I}^2}. According to Nelsen, letting \eqn{\mathcal{J}} denote a \emph{partition} of \eqn{\mathcal{I}^2} and \eqn{\mathcal{J}_i = [a_i,\, b_i]} be the \eqn{i}th partition that does not overlap with others and letting also \eqn{\mathbf{C}_i} be a copula for the \eqn{i}th partition, then the \emph{ordinal sum} of these \eqn{\mathbf{C}_i} with parameters \eqn{\Theta_i} \emph{with respect to} \eqn{\mathcal{J}_i} is the copula \eqn{\mathbf{C}} given by

\deqn{\mathbf{C}\bigl(u,v; \mathcal{J}_i, \mathbf{C}_i, \Theta_i, i \in 1,2,\cdots,n\bigr) = a_i + (b_i-a_i)\mathbf{C}_i\biggl(\frac{u-a_i}{b_i-a_i},\, \frac{v-a_i}{b_i-a_i};  \Theta_i\biggr)\  \mbox{for}\ (u,v) \in \mathcal{J}^2\mbox{,}}

for points within the partitions, and for points otherwise outside the partitions the coupla is given by

\deqn{\mathbf{C}\bigl(u,v; \mathcal{J}_i, \mathbf{C}_i, i \in 1,2,\cdots,n\bigr) = \mathbf{M}(u,v)\  \mathrm{for}\ (u,v) \ni \mathcal{J}^2\mbox{, and}}

let \eqn{\mathbf{C}_\mathcal{J}(u,v)} be a convenient abbreviation for the copula. Finally, Nelsen (2006, theorem 3.2.1) states that a copula is an ordinal sum if and only if for a \eqn{t} if \eqn{\mathbf{C}(t,t)=t} for \eqn{t \in (0,1)}. The \emph{diagonal of a coupla} can be useful for quick assessment (see \bold{Examples}) of this theorem. (See \code{\link{ORDSUWcop}}, \emph{W-ordinal sum of the summands}.)
}
\usage{
ORDSUMcop(u,v, para=list(cop=W, para=NA, part=c(0,1)), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A list of sublists for the coupla, parameters, and partitions (see \bold{Examples}) and some attempt for intelligent in-fill of \code{para} is made within the sources (the default \code{para} is an example for which \code{cop} and \code{para} elements are converted to lists). The user is responsible that \code{part} element properly canvases by end-point alignment all of \eqn{\mathcal{I}^2}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Klement, E.P., \enc{Kolesárová}{Kolesarova}, A., Mesiar, R., Saminger-Platz, S., 2017, Copula constructions using ultramodularity (chap. 9) \emph{in} Copulas and dependence models with applications---Contributions in honor of Roger B. Nelsen, \emph{eds.} Flores, U.M., Amo Artero, E., Durante, F., \enc{Sánchez}{Sanchez}, J.F.: Springer, Cham, Switzerland, ISBN 978--3--319--64220--9, \doi{10.1007/978-3-319-64221-5}.
}
\author{W.H. Asquith}
\seealso{\code{\link{copBasic-package}}, \code{\link{W_N5p12a}}, \code{\link{ORDSUWcop}}}
\examples{
\dontrun{
  para <- list(cop=c(CLcop, M, PLcop, GHcop), para=list(4, NA, 0.1, c(3, 4)),
               part=list(c(0, 0.25), c(0.25, 0.35), c(0.35, 0.85), c(0.85, 1)))
  UV <- simCOP(n=100, cop=ORDSUMcop, para=para, ploton=FALSE)
  plot(c(0,1), c(0,1), xlab="U, nonexceedance probability", type="n", las=1,
                       ylab="V, nonexceedance probability")
  for(k in seq_len(length(para$part))) {         #  to draw the partitions
    a <- para$part[[k]][1]; b <- para$part[[k]][2]
    polygon(c(a, b, b, a, a), c(a, a, b, b, a), lty=2, lwd=0.8, col="lightgreen")
    text( (a+b) / 2,  (a+b) / 2, k, cex=3, col="blue") # numbered by partition
  }
  points(UV, pch=21, cex=0.8, col=grey(0.1), bg="white") #}

\dontrun{
  para <- list(cop=c(GHcop), para=list(c(2, 3)), # internally replicated
               part=list(c(0, 0.2), c(0.2, 0.3), c(0.3, 0.5), c(0.5, 0.7), c(0.7, 1)))
  UV <- simCOP(n=100, cop=ORDSUMcop, para=para, ploton=FALSE)
  plot(c(0,1), c(0,1), xlab="U, nonexceedance probability", type="n", las=1,
                       ylab="V, nonexceedance probability")
  for(k in seq_len(length(para$part))) {         #  to draw the partitions
    a <- para$part[[k]][1]; b <- para$part[[k]][2]
    polygon(c(a, b, b, a, a), c(a, a, b, b, a), lty=2, lwd=0.8, col="lightgreen")
    text( (a+b) / 2,  (a+b) / 2, k, cex=3, col="blue") # numbered by partition
  }
 points(UV, pch=21, cex=0.8, col=grey(0.1), bg="white") #}

\dontrun{
  # In this example, it is important that the delt is of the resolution
  # matching the edges of the partitions.
  para <- list(cop=P, para=list(NULL),
               part=list(c(0, 0.257), c(0.257, 0.358), c(0.358, 1)))
  DI <- diagCOP(cop=ORDSUMcop, para=para, delt=0.001)
  if(sum(DI$diagcop == DI$t) >= 1) {
    message("The ORDSUMcop() operation is an ordinal sum if there exists\n",
            "a t=(0,1) exists such that C(t,t)=t by Nelsen (2006, theorem 3.2.1).")
  }
  lines(par()$usr[1:2], par()$usr[3:4], col="red") #}

\dontrun{
  # Expansive demonstration of high levels of "shape" of bivariate relations beyond
  # the "simple" measures of association (Rho, Tau, Wolf, ...):
  myCop <- ORDSUMcop # Note, integration problems more likely, if changed to
  # ORDSUWcop at which point |Spearman Rhos| might integrate to beyond unity.
  # Note, hardwired parameters in the demo-definitions of custom subcouplas to
  # avoid confusion to study departure away from true M() and W() copulas, and to
  # have these examples producing permutation asymmetry and L-comoments not exchangable
  # though only the with respect to UV [12] being plotted. We swap to M, W as shown
  # in the example (three lines below) to make the default of example exchangable.
  myM <- function(u,v, para=NULL, ...)         P(u,   v, para=NULL, ...)
  myW <- function(u,v, para=NULL, ...) u - PAcop(u, 1-v, para=7, ...) # rotate 90d
  myM <- M; myW <- W # comment this line out for "custom subcopulas" after which
  # the "myM" means first defined subcopula and "myW" means second defined subcopula.
  # ------------- Demonstrate with mid-partioning -------------
  mara <- list(cop=c(myM, myW),  para=NULL, part=c(0, 0.5, 1))
  UVm  <- simCOP(1E3, cop=myCop, para=mara, seed=1, col="red")
  wara <- list(cop=c(myW, myM),  para=NULL, part=c(0, 0.5, 1))
  UVw  <- simCOP(1E3, cop=myCop, para=wara, seed=1, col="blue", ploton=FALSE)
  cor(UVm[,1], UVm[,2], method="spearman"); cor(UVw[,1], UVw[,2], method="spearman")
  # -----------------------------------------------------------
  opts <- par(las=1, lend=1, ljoin=1) # all lab horz, butted, mitred
  del  <- 0.005
  nsim <- 300; ps <- c(0.001, seq(del, 1-del, by=del), 0.999)
  # The ylim might need extension, it seems cases of L-comoments exceeding unity
  # can be found dependening on the settings of the myM and myW.
  plot(c(0, 1), c(-1, 1), type="n", ylab="Spearman Rho, L-coskew, and L-cokurtosis",
       xlab="Ordinal sum partioning parameter")
  # -- Stuff below can be run again with changing the three logical
  show_points     <- FALSE # variables as user needs. We like TRUE
  show_guidelines <- TRUE  # FALSE, FALSE for a look by sampling
  dolcomCOP       <- TRUE  # before FALSE, TRUE, TRUE for theory.
  T2m12 <- T3m12 <- T4m12 <- T2w12 <- T3w12 <- T4w12 <- NULL # L-comoments
  T2m21 <- T3m21 <- T4m21 <- T2w21 <- T3w21 <- T4w21 <- NULL # L-comoments
  NuSk_m <- NuSt_m <- NuSk_w <- NuSt_w <- NULL # Joe "skews"
  # lcomCOP()          # theoretical L-comoments by integration
  # lmomco::lcomoms2() # sample L-comoments estimation
  # nuskewCOP() is a permutation asymmetry measure and nustarCOP() is something
  # slightliy more complex in what it measures, but it is important to note that
  # each is in a linear combination to the dual integral of C(u,v), and therefore,
  # we can expect them to be coupled to Spearman Rho itself in a rather simple way.
  for(p in ps) { message(p, "-", appendLF=FALSE)
    mara <- list(cop=c(myM, myW), para=NULL, part=c(0, p, 1))
    wara <- list(cop=c(myW, myM), para=NULL, part=c(0, p, 1))
    if(dolcomCOP) { # theoretical (numerical integration)
      lcm    <- lcomCOP(cop=myCop, para=mara, stop.on.error=FALSE)
      lcw    <- lcomCOP(cop=myCop, para=wara, stop.on.error=FALSE)
      T2m12  <- c(T2m12, lcm$lcomUV[2]); T2w12 <- c(T2w12, lcw$lcomUV[2])
      T2m21  <- c(T2m21, lcm$lcomVU[2]); T2w21 <- c(T2w21, lcw$lcomVU[2])
      T3m12  <- c(T3m12, lcm$lcomUV[3]); T4m12 <- c(T4m12, lcm$lcomUV[4])
      T3w12  <- c(T3w12, lcw$lcomUV[3]); T4w12 <- c(T4w12, lcw$lcomUV[4])
      T3m21  <- c(T3m21, lcm$lcomVU[3]); T4m21 <- c(T4m21, lcm$lcomVU[4])
      T3w21  <- c(T3w21, lcw$lcomVU[3]); T4w21 <- c(T4w21, lcw$lcomVU[4])
      NuSk_m <- c(NuSk_m, nuskewCOP( cop=myCop, para=mara) )
      NuSt_m <- c(NuSt_m, nustarCOP( cop=myCop, para=mara) )
      NuSk_w <- c(NuSk_w, nuskewCOP( cop=myCop, para=wara) )
      NuSt_w <- c(NuSt_w, nustarCOP( cop=myCop, para=wara) )
    } else {        # sample versions
      uvm    <- rCOP(nsim, cop=myCop, para=mara, seed=1)
      uvw    <- rCOP(nsim, cop=myCop, para=wara, seed=1)
      lcm    <- lmomco::lcomoms2(uvm, nmom=4)
      lcm    <- list(lcomUV=c(NA, lcm$T2[1,2], lcm$T3[1,2], lcm$T4[1,2]),
                     lcomVU=c(NA, lcm$T2[2,1], lcm$T3[2,1], lcm$T4[2,1]))
      lcw    <- lmomco::lcomoms2(uvw, nmom=4)
      lcw    <- list(lcomUV=c(NA, lcw$T2[1,2], lcw$T3[1,2], lcw$T4[1,2]),
                     lcomVU=c(NA, lcw$T2[2,1], lcw$T3[2,1], lcw$T4[2,1]))
      T2m12  <- c(T2m12, lcm$lcomUV[2]); T2w12 <- c(T2w12, lcw$lcomUV[2])
      T2m21  <- c(T2m21, lcm$lcomVU[2]); T2w21 <- c(T2w21, lcw$lcomVU[2])
      T3m12  <- c(T3m12, lcm$lcomUV[3]); T4m12 <- c(T4m12, lcm$lcomUV[4])
      T3w12  <- c(T3w12, lcw$lcomUV[3]); T4w12 <- c(T4w12, lcw$lcomUV[4])
      T3m21  <- c(T3m21, lcm$lcomVU[3]); T4m21 <- c(T4m21, lcm$lcomVU[4])
      T3w21  <- c(T3w21, lcw$lcomVU[3]); T4w21 <- c(T4w21, lcw$lcomVU[4])
      NuSk_m <- c(NuSk_m, nuskewCOP( para=uvm, as.sample=TRUE) )
      NuSt_m <- c(NuSt_m, nustarCOP( para=uvm, as.sample=TRUE) )
      NuSk_w <- c(NuSk_w, nuskewCOP( para=uvw, as.sample=TRUE) )
      NuSt_w <- c(NuSt_w, nustarCOP( para=uvw, as.sample=TRUE) )
    }
    # -----------------------------------------------------
    if(show_points) {
      n <- length(T3m12)
      points(p,  T2m12[n], pch="R", col="seagreen4"  ) # Spearman Rho
      points(p,  T2w12[n], pch="R", col="darkorchid4") # Spearman Rho
      points(p,  T3m12[n], pch=24,  col="seagreen4",   bg="seagreen4"  )
      points(p,  T4m12[n], pch=25,  col="seagreen4",   bg="seagreen4"  )
      points(p,  T3w12[n], pch=24,  col="darkorchid4", bg="darkorchid4")
      points(p,  T4w12[n], pch=25,  col="darkorchid4", bg="darkorchid4")
      points(p, NuSk_m[n], pch=2,   col="deepskyblue2") # open upright tri
      points(p, NuSt_m[n], pch=8,   col="seagreen4"   ) # pch == star
      points(p, NuSk_w[n], pch=6,   col="deepskyblue2") # open flipped tri
      points(p, NuSt_w[n], pch=8,   col="darkorchid4" ) # pch == star
    }
  }
  message("done")
  if(show_guidelines) {
    psna <- c(ps, NA, ps) # insert NA to "line break"
    lines(psna, c( T2m12, NA,  T2w12), lwd=3, col="grey80")
    lines(psna, c( T3m12, NA,  T3w12), lty=1)
    lines(psna, c( T4m12, NA,  T4w12), lty=2)
    lines(psna, c(NuSk_m, NA, NuSk_w), lty=1)
    lines(psna, c(NuSt_m, NA, NuSt_w), lty=2)
  }
  par(opts)
  # Note, the nuskews will plot along origin line if the "image" of the two
  # subcouplas are permutation symmetric and result in a hexagram (six-
  # sided star) being "plotted." If subcopulas themselves are not permutation
  # symmetric, then triangles for nuskew will diverge from each other.}

\dontrun{
  # Now, look at the coupling between the Spearman Rho and the L-comoments through
  # formation of L-comoment ratio diagrams of the ordindal sums.
  # Here, we just draw lines from whatever the vectors stemming from the previous
  # example produced by dolcomCOP being set TRUE (theoretical) or FALSE (sample)
  # L-comoments. For the simple, ordinal sum of M,W or W,M, then L-cokurtosis
  # will plot on top of each other; therefore, we use the thick line for T4m12.
  plot(c(-1, 1), c(-1, 1), type="n", las=1,
       xlab="Spearman Rho (driven by ordinal sum parition)"
       ylab="L-coskew (cT3) or L-cokurtosis (cT4)")
  lines(par()$usr[1:2], rep(0, 2))
  lines(T2m12, T3m12, lty=1, col="seagreen4", lwd=3)
  lines(T2m12, T4m12, lty=2, col="seagreen4", lwd=3)
  lines(T2w12, T3w12, lty=1, col="darkorchid4")
  lines(T2w12, T4w12, lty=2, col="darkorchid4") # (myM,myW);(myW,myM) plot on T4m21?
  txt <- c("L-coskew cT3[12], cop(myM, myW)", "L-cokurtosis cT4[12], cop(myM, myW)",
           "L-coskew cT3[12], cop(myW, myM)", "L-cokurtosis cT4[12], cop(myW, myM)")
  legend("topleft", txt, bty="n", seg.len=3, lty=c(1, 2, 1, 2), lwd=c(3, 3, 1, 1),
         col=c("seagreen4", "seagreen4", "darkorchid4", "darkorchid4"))
  # --------------------------
  # If the user is working with sufficiently complex copulas, then differing
  # trajectories through the domain occur.
  plot(c(-1, 1), c(-1, 1), type="n", las=1, ylab="L-coskew or L-cokurtosis",
       xlab="Spearman Rho (driven by ordinal sum parition)")
  lines(par()$usr[1:2], rep(0, 2))
  lines(T2m21, T3m21, lty=1, col="seagreen4", lwd=3)
  lines(T2m21, T4m21, lty=2, col="seagreen4", lwd=3)
  lines(T2w21, T3w21, lty=1, col="darkorchid4")
  lines(T2w21, T4w21, lty=2, col="darkorchid4") # (myM,myW);(myW,myM) plot on T4m21?
  txt <- c("L-coskew cT3[21], cop(myM, myW)", "L-cokurtosis cT4[21], cop(myM, myW)",
           "L-coskew cT3[21], cop(myW, myM)", "L-cokurtosis cT4[21], cop(myW, myM)")
  legend("topleft", txt, bty="n", seg.len=3, lty=c(1, 2, 1, 2), lwd=c(3, 3, 1, 1),
         col=c("seagreen4", "seagreen4", "darkorchid4", "darkorchid4")) # }

\dontrun{
  # --------------------------
  # Next, let us build polynomial approximations for the boundaries and the polynomial
  # degree has been set pretty large in order that the user would have built-in
  # flexibility to construction of these approximations for more complex copula than
  # the stacking of M,W and W,M.
  X    <- c(T2m12, T2m21); Y3mw <- c(T3m12, T3m21); xs <- seq(-1,1, by=0.001)
  T3fT2mw <- lm(Y3mw~I(X^ 1)+I(X^ 2)+I(X^ 3)+I(X^ 4)+I(X^ 5)+I(X^ 6)+
                     I(X^ 7)+I(X^ 8)+I(X^ 9)+I(X^10)+I(X^11)+I(X^12))
  plot(X, Y3mw, xlab="Spearman Rho", ylab="L-coskew (cT3)")
  lines(xs, predict(T3fT2mw, newdata=data.frame(X=xs)), col="red")
  mtext("2-partition ORDSUMcop : M, W")

  X    <- c(T2w12, T2w21); Y3wm <- c(T3w12, T3w21)
  T3fT2wm <- lm(Y3wm~I(X^ 1)+I(X^ 2)+I(X^ 3)+I(X^ 4)+I(X^ 5)+I(X^ 6)+
                     I(X^ 7)+I(X^ 8)+I(X^ 9)+I(X^10)+I(X^11)+I(X^12))
  plot(X, Y3wm, xlab="Spearman Rho", ylab="L-coskew (cT3)")
  lines(xs, predict(T3fT2wm, newdata=data.frame(X=xs)), col="red")
  mtext("2-partition ORDSUMcop : W, M")

  X  <- c(T2m12, T2m21, T2w12, T2w21); Y4 <- c(T4m12, T4m21, T4w12, T4w21)
  T4fT2mwwm <- lm(Y4~I(X^ 1)+I(X^ 2)+I(X^ 3)+I(X^ 4)+I(X^ 5)+I(X^ 6)+
                     I(X^ 7)+I(X^ 8)+I(X^ 9)+I(X^10)+I(X^11)+I(X^12))

  plot(X, Y4, xlab="Spearman Rho", ylab="L-cokurtosis (cT4)")
  lines(xs, predict(T4fT2mwwm, newdata=data.frame(X=xs)), col="red")

  X  <- c(T3m12, T3m21, T3w12, T3w21)
  Y4 <- c(T4m12, T4m21, T4w12, T4w21)
  plot(X, Y4, type="l", xlab="L-coskew", ylab="L-cokurtosis")
  points(T3m12, T4m12, col="seagreen4")
  points(T3w12, T4w12, col="orchid3"  ) # }

\dontrun{
  # --------------------------
  # Next, let us extract the coefficients and build-up stand alone functions that
  # could be used to draw the L-comoments of the ordinal sums and potentially
  # overlaid on L-comoment ratio diagrams such as seen built under the Examples in the
  # LCOMDIA*.Rd documentation files (for instance LCOMDIA_ManyCops.Rd) of the package.
  coesA <- T3fT2mw$coefficients;   names(coesA) <- NULL
  coesB <- T3fT2wm$coefficients;   names(coesB) <- NULL
  coesC <- T4fT2mwwm$coefficients; names(coesC) <- NULL
  print(coesA, 12); print(coesB, 12); print(coesC, 12)
  coesT3 <- sapply(seq_len(length(coesA)), function(i) {
               mean(c(abs(coesA[i]), abs(coesB[i]))) })
  print(coesT3, 12)
  "T34fT2" <- function(rho, type=c("T3mw", "T3wm", "T4mwwm")) {
    type <- match.arg(type)
    cs3 <- c(0.618673707079,
             0.175958850204, 0.512576942435, 0.145431255262, 0.265479078036,
             0.163312255777, 1.016327127995, 0.537153927577, 2.225271502140,
             0.636080402864, 2.191529289514, 0.292183399458, 0.823239656100)
    if(type == "T3mw") {
      cs <- c(-1,-1, 1, 1, 1,-1,-1, 1, 1,-1,-1, 1, 1)*cs3
    } else if(type == "T3wm") {
      cs <- c( 1, 1,-1,-1,-1, 1, 1,-1,-1, 1, 1,-1,-1)*cs3
    } else if(type == "T4mwwm") {
      cs <- c( 0.236681925424,
              -0.429191591357, -0.573421581827, 0.352083105242, 1.042508365628,
              -0.710792468014, -4.321529301154, 2.229039543342, 9.419379550877,
              -2.652104491697, -9.275892279321, 1.208446517629, 3.472344853036)
      snmw <- rep(1, length(cs))
    } else {
      stop("should not be here in logic")
    }
    z <- sapply(rho, function(r) sum(sapply(seq_len(length(cs)),
                                     function(p) cs[p] * r^(p-1))))
    return(z)
  }

  # Finally, plot again the L-comoment ratio diagram of Spearman Rho and the
  # two higher L-comoments and a L-coskew and L-cokurtosis diagram could also be
  # readily built from a vector of Spearman Rhos.
  ylim <- c(-0.7, 0.7)
  plot( xs, T34fT2(xs, "T3mw"), type="n", ylim=ylim,
       xlab="Spearman Rho", ylab="L-coskew or L-cokurtosis approximations")
  lines(par()$usr[1:2], rep(0, 2))
  lines(xs, T34fT2(xs, "T3mw"),   col="brown3")
  lines(xs, T34fT2(xs, "T3wm"),   col="blue3" )
  lines(xs, T34fT2(xs, "T4mwwm"), col="orange")
  # --------------------------
  ylim <- c(-0.7, 0.7)
  plot(c(T34fT2(xs, "T3mw"  ), T34fT2(xs, "T3wm"  )),
       c(T34fT2(xs, "T4mwwm"), T34fT2(xs, "T4mwwm")), type="n", ylim=ylim,
       xlab="L-coskew approximations", ylab="L-cokurtosis approximations")
  lines(par()$usr[1:2], rep(0, 2)); lines(rep(0, 2), par()$usr[3:4])
  lines(T34fT2(xs, "T3mw"), T34fT2(xs, "T4mwwm"),      col="seagreen4")
  lines(T34fT2(xs, "T3wm"), T34fT2(xs, "T4mwwm"),      col="orchid3"  )
  text(-0.35, 0.07, "Ordinal sum of\nM and W", font=2, col="seagreen4")
  text(+0.35, 0.07, "Ordinal sum of\nW and M", font=2, col="orchid3"  ) # }
}
\keyword{copula (formulas)}
\concept{Ordinal Sums}
\keyword{copula}
\keyword{ordinal sums}
\keyword{M-ordinal sum of the summands}
\concept{summands}
\keyword{L-comoments (ratio diagram)}
\keyword{L-comoment ratio diagram}
