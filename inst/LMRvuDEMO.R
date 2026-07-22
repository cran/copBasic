library(copBasic)
library(lmomco)
Lru <- function(u, r=2) { # eq. 15b Jones (2004) and Asquith (2011, 6.10 is wrong with 1/(1-r) and needs 1/(r-1))
  sapply(u, function(u) { sum(sapply(0:(r-2), function(j) {
                  (-1)^j * exp(lchoose(r-1,j)+lchoose(r-1,j+1))*u^(r-2-j)*(1-u)^j })) })/(r-1)
}

para <- vec2par(c(-10,0,10), type="tri")
integrate(function(x, para=para) { u <- cdftri(x, para=para)
       return(u*(1-u)*Lru(u, r=2)) }, -100, +100, para=para) # 7/3
integrate(function(x, para=para) { u <- cdftri(x, para=para)
       return(u*(1-u)*Lru(u, r=2)) }, -Inf, +Inf, para=para) # 7/3
lmomtri(para)$lambdas[2] # 7/3
integrate(function(x, para=para) { u <- cdftri(x, para=para)
       return(u*(1-u)*Lru(u, r=4)) }, -100, +100, para=para) # 0.2103149
integrate(function(x, para=para) { u <- cdftri(x, para=para)
       return(u*(1-u)*Lru(u, r=4)) }, -Inf, +Inf, para=para) # 0.2103149
lmomtri(para)$lambdas[4] # 0.2103149


para <- list(cop=breveCOP, para=list(cop=CLcop, para=10, breve=0.2, reflect=2))
uv <- simCOP(500, cop=COP, para=para, graphics=FALSE, set.seed=1)

ustar <- 0.96; set.seed(1)
v_special <- replicate(10000, simCOPv(ustar, cop=COP, para=para))
lmr <- lmomco::lmoms(v_special)

nsim <- 1e+03
u <- seq(0.001,1, by=0.0005)
medvatu <- med.regressCOP(u, cop=COP, para=para)[,2]
lam1atu <- EvuCOP(u, cop=COP, para=para)
lam2atu <- sapply(u, function(u) { E <- NULL
                               try(E <- integrate(function(v, para=para) {
              t <- derCOP(cop=COP, u, v, para=para); return(t*(1-t)*Lru(t, r=2)) }, 0, +1,
                                         para=para, subdivisions=200)$value, silent=TRUE)
                        if(is.null(E)) {
                                        t <- derCOP(cop=COP, u, runif(nsim), para=para)
                                   E <- mean(t*(1-t)*Lru(t, r=2))
                             names(E) <- "sim"
                        }
                            return(E) })
lam3atu <- sapply(u, function(u) { E <- NULL
                               try(E <- integrate(function(v, para=para) {
              t <- derCOP(cop=COP, u, v, para=para); return(t*(1-t)*Lru(t, r=3)) }, 0, +1,
                                         para=para, subdivisions=200)$value, silent=TRUE)
                        if(is.null(E)) {
                                        t <- derCOP(cop=COP, u, runif(nsim), para=para)
                                   E <- mean(t*(1-t)*Lru(t, r=3))
                             names(E) <- "sim"
                        }
                            return(E) })
lam4atu <- sapply(u, function(u) { E <- NULL
                               try(E <- integrate(function(v, para=para) {
               t <- derCOP(cop=COP, u, v, para=para); return(t*(1-t)*Lru(t, r=4)) }, 0, +1,
                                          para=para, subdivisions=200)$value, silent=TRUE)
                        if(is.null(E)) {
                                        t <- derCOP(cop=COP, u, runif(nsim), para=para)
                                   E <- mean(t*(1-t)*Lru(t, r=4))
                             names(E) <- "sim"
                        }
                            return(E) })

#pdf("LMRvuCOP.pdf", height=7, width=7, useDingbats=FALSE)
par(xpd=NA, las=1, lend=1, col.axis="black", cex.axis=0.9,
    mgp=c(2,0.6,0), mai=c(0.65,0.65,0.40,0.65))
plot(uv[,1], uv[,2], type="n", lwd=0.8, col="grey50", bty="n", xlim=c(0,1), ylim=c(0,1),
      xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
lines(rep(ustar, 2), par()$usr[3:4], lty=4, lwd=1.2, col="seagreen4")
par(mgp=c(2,0.3,0), las=0, col.axis="seagreen4", cex.axis=0.8)
axis(1, at=ustar, label=latex2exp::TeX(r'($u^\star$)'), lwd=0, lwd.ticks=1.2, tcl=-0.25, col.ticks="seagreen4")
axis(3, at=ustar, label=latex2exp::TeX(r'($u^\star$)'), lwd=0, lwd.ticks=1.2, tcl=-0.25, col.ticks="seagreen4")
axis(1, at=ustar, label=FALSE,   lwd=0, lwd.ticks=1.2, tcl=+0.25, col.ticks="seagreen4")
axis(3, at=ustar, label=FALSE,   lwd=0, lwd.ticks=1.2, tcl=+0.25, col.ticks="seagreen4")
par(mgp=c(2,0.6,0), las=1, col.axis="black", cex.axis=0.9)
points(uv[,1], uv[,2], lwd=0.9, pch=21, cex=0.8, col="wheat4", bg="wheat1")
axis(1, at=axTicks(1), label=TRUE,  lwd=1, lwd.ticks=1, col.ticks="black")
axis(3, at=axTicks(1), label=FALSE, lwd=1, lwd.ticks=1, col.ticks="black")
tix <- seq(0.1, 0.9, by=0.2)
axis(1, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
axis(2, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
axis(3, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
tix <- seq(0.05, 0.95, by=0.05); tix <- tix[grep("5$", as.character(tix))]
axis(1, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
axis(2, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
axis(3, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
mtext("U, nonexceedance probability", side=1, line=1.6)
par(mgp=c(2,0.8,0))
axis(2, at=axTicks(2), label=TRUE,  lwd=1, lwd.ticks=1, col.ticks="black")
par(las=0)
mtext("V, nonexceedance probability", side=2, line=2.1)
par(las=1)
lines(u, lam1atu, lwd=2,  col="black"  ) # mean probability
lines(u, medvatu, lwd=2,  col="red1"   ) # median probability
lines(u, lam2atu, lwd=2,  col="deepskyblue3") # conditional L-scale
points(ustar, mean(  v_special), cex=1.7, pch=1,  col="black"  )
points(ustar, median(v_special), cex=1.7, pch=1,  col="red1"   )
points(ustar, lmr$lambdas[1],    cex=1.3, pch=16, col="black"  )
points(ustar, lmr$lambdas[2],    cex=1.3, pch=16, col="deepskyblue3")
par(new=TRUE)
plot(u, lam3atu/lam2atu, type="n", bty="n",xlim=c(0,1), ylim=c(-1, 1),
         xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
par(col.axis="orchid3", las=0, lend=1)
mtext("Lskew and Lkurtosis, dimensionless", 4, line=2.15, col="orchid3")
par(las=1)
axis(4, at=axTicks(2), label=TRUE, lwd=2, lwd.ticks=1, col="orchid3", col.ticks="orchid3")
tix <- seq(-0.9, 0.9, by=0.1); tix <- tix[grep("[05]$", as.character(tix), invert=TRUE)]
axis(4, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="orchid3", tcl=-0.5)

lines(u, lam3atu/lam2atu, col="orchid3", lwd=2) # conditional L-skew
lines(u, lam4atu/lam2atu, col="orchid3", lty=2, lwd=2) # conditional L-kurtosis
points(ustar, lmr$ratios[ 3],    cex=1.2,   pch=24, col="orchid3", bg="orchid1")
points(ustar, lmr$ratios[ 4],    cex=1.2,   pch=25, col="orchid3", bg="orchid1")

txt <- c(paste0("Conditional (Cond.) mean at ustar = ", ustar, ", EvuCOP(...)"),
         "Cond. median at ustar, med.regressCOP(...)",
         "Cond. Lscale (variation) at ustar, LMRvuCOP(...)",
         "Cond. Lskew at ustar, LMRvuCOP(...)",
         "Cond. Lkurtosis at ustar, LMRvuCOP(...)",
         "Demonstration of n=500 sim. values for the copula",
         "Cond. simulated (sim.) (n=10,000) sample mean, mean(...)",
         "Cond. sim. sample median, median(...)",
         "Cond. sim. sample mean, lmoms(...)$lambdas[1]",
         "Cond. sim. sample Lscale, lmoms(...)$lambdas[2]",
         "Cond. sim. sample Lskew, lmoms(...)$ratios[3]",
         "Cond. sim. sample Lkurtosis, lmoms(...)$ratios[4]")
legend("topleft", txt, bty="n", cex=0.6, seg.len=4, y.intersp=1.3,
        lwd=c(2,2,2,2,2, NA, rep(NA, 6)), lty=c(1,1,1,1,2,0,rep(0, 6)),
        pch=c(rep(NA, 5), 21, 1, 1, 16, 16, 24, 25),
        pt.lwd=c(rep(NA,5), 0.9, rep(1,6)),
        pt.cex=c(rep(NA,5), 0.8, 1.7, 1.7, 1.3, 1.3, 1.2, 1.2),
        pt.bg=c(rep(NA,5), "wheat1", rep(NA,4), "orchid1", "orchid1"),
        col=c("black", "red1", "deepskyblue3", "orchid3", "orchid3",
              "wheat4", "black", "red1", "black", "deepskyblue3", "orchid3", "orchid3"))
par(xpd=NA, las=1, lend=1, col.axis="black", cex.axis=0.9,
    mgp=c(2,0.6,0), mai=c(0.65,0.65,0.40,0.65))
#dev.off()



para <- list(cop=breveCOP, para=list(cop=CLcop, para=c(10), breve=0.2, reflect=2))
uv <- simCOP(500, cop=COP, para=para, graphics=FALSE, set.seed=1)

ustar <- 0.96; set.seed(1)
v_special <- replicate(10000, simCOPv(ustar, cop=COP, para=para))
lmr <- lmomco::lmoms(v_special)


u <- v <- seq(0,1, by=0.001)
LMRvu <- LMRvuCOP(u, cop=COP, para=para)
LMRuv <- LMRuvCOP(v, cop=COP, para=para)

#uv <- uv[,2:1]
#LMRvu <- LMRuv
#pdf("LMRvuCOP.pdf", height=7, width=7, useDingbats=FALSE)
par(xpd=NA, las=1, lend=1, col.axis="black", cex.axis=0.9,
    mgp=c(2,0.6,0), mai=c(0.65,0.65,0.40,0.65))
plot( uv[,1], uv[,2], type="n", lwd=0.8, col="grey50", bty="n", xlim=c(0,1), ylim=c(0,1),
      xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
lines(rep(ustar, 2), par()$usr[3:4], lty=4, lwd=1.2, col="seagreen4")
par(mgp=c(2,0.3,0), las=0, col.axis="seagreen4", cex.axis=0.8)
axis(1, at=ustar, label=latex2exp::TeX(r'($u^\star$)'), lwd=0, lwd.ticks=1.2, tcl=-0.25, col.ticks="seagreen4")
axis(3, at=ustar, label=latex2exp::TeX(r'($u^\star$)'), lwd=0, lwd.ticks=1.2, tcl=-0.25, col.ticks="seagreen4")
axis(1, at=ustar, label=FALSE,   lwd=0, lwd.ticks=1.2, tcl=+0.25, col.ticks="seagreen4")
axis(3, at=ustar, label=FALSE,   lwd=0, lwd.ticks=1.2, tcl=+0.25, col.ticks="seagreen4")
par(mgp=c(2,0.6,0), las=1, col.axis="black", cex.axis=0.9)
points(uv[,1], uv[,2], lwd=0.9, pch=21, cex=0.8, col="wheat4", bg="wheat1")
axis(1, at=axTicks(1), label=TRUE,  lwd=1, lwd.ticks=1, col.ticks="black")
axis(3, at=axTicks(1), label=FALSE, lwd=1, lwd.ticks=1, col.ticks="black")
tix <- seq(0.1, 0.9, by=0.2)
axis(1, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
axis(2, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
axis(3, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
tix <- seq(0.05, 0.95, by=0.05); tix <- tix[grep("5$", as.character(tix))]
axis(1, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
axis(2, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
axis(3, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
mtext("U, nonexceedance probability", side=1, line=1.6)
par(mgp=c(2,0.8,0))
axis(2, at=axTicks(2), label=TRUE,  lwd=1, lwd.ticks=1, col.ticks="black")
par(las=0)
mtext("V, nonexceedance probability", side=2, line=2.1)
par(las=1)
lines(u, LMRvu$L1,     lwd=2, col="black"  ) # mean probability
lines(u, LMRvu$median, lwd=2, col="red1"   ) # median probability
lines(u, LMRvu$L2,     lwd=2, col="deepskyblue3") # conditional L-scale
points(ustar,   mean(v_special), cex=1.7, pch=1,  col="black"  )
points(ustar, median(v_special), cex=1.7, pch=1,  col="red1"   )
points(ustar, lmr$lambdas[1],    cex=1.3, pch=16, col="black"  )
points(ustar, lmr$lambdas[2],    cex=1.3, pch=16, col="deepskyblue3")
par(new=TRUE)
plot(u, LMRvu$T3, type="n", bty="n",xlim=c(0,1), ylim=c(-1, 1),
         xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
par(col.axis="orchid3", las=0, lend=1)
mtext("Lskew and Lkurtosis, dimensionless", 4, line=2.15, col="orchid3")
par(las=1)
axis(4, at=axTicks(2), label=TRUE, lwd=2, lwd.ticks=1, col="orchid3", col.ticks="orchid3")
tix <- seq(-0.9, 0.9, by=0.1); tix <- tix[grep("[05]$", as.character(tix), invert=TRUE)]
axis(4, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="orchid3", tcl=-0.5)

lines(u, LMRvu$T3, col="orchid3", lwd=2) # conditional L-skew
lines(u, LMRvu$T4, col="orchid3", lty=2, lwd=2) # conditional L-kurtosis
points(ustar, lmr$ratios[ 3],    cex=1.2,   pch=24, col="orchid3", bg="orchid1")
points(ustar, lmr$ratios[ 4],    cex=1.2,   pch=25, col="orchid3", bg="orchid1")

txt <- c(paste0("Conditional (Cond.) mean at ustar = ", ustar, ", EvuCOP(...)"),
         "Cond. median at ustar, med.regressCOP(...)",
         "Cond. Lscale (variation) at ustar, LMRvuCOP(...)",
         "Cond. Lskew at ustar, LMRvuCOP(...)",
         "Cond. Lkurtosis at ustar, LMRvuCOP(...)",
         "Demonstration of n=500 sim. values for the copula",
         "Cond. simulated (sim.) (n=10,000) sample mean, mean(...)",
         "Cond. sim. sample median, median(...)",
         "Cond. sim. sample mean, lmoms(...)$lambdas[1]",
         "Cond. sim. sample Lscale, lmoms(...)$lambdas[2]",
         "Cond. sim. sample Lskew, lmoms(...)$ratios[3]",
         "Cond. sim. sample Lkurtosis, lmoms(...)$ratios[4]")
legend("topleft", txt, bty="n", cex=0.6, seg.len=4, y.intersp=1.3,
        lwd=c(2,2,2,2,2, NA, rep(NA, 6)), lty=c(1,1,1,1,2,0,rep(0, 6)),
        pch=c(rep(NA, 5), 21, 1, 1, 16, 16, 24, 25),
        pt.lwd=c(rep(NA,5), 0.9, rep(1,6)),
        pt.cex=c(rep(NA,5), 0.8, 1.7, 1.7, 1.3, 1.3, 1.2, 1.2),
        pt.bg=c(rep(NA,5), "wheat1", rep(NA,4), "orchid1", "orchid1"),
        col=c("black", "red1", "deepskyblue3", "orchid3", "orchid3",
              "wheat4", "black", "red1", "black", "deepskyblue3", "orchid3", "orchid3"))
par(xpd=NA, las=1, lend=1, col.axis="black", cex.axis=0.9,
    mgp=c(2,0.6,0), mai=c(0.65,0.65,0.40,0.65))
#dev.off()

