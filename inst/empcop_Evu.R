library(lmomco)
xpara <- lmomco::vec2par(c(3, 0.6, +0.4), type="pe3")
ypara <- lmomco::vec2par(c(3, 0.4, -0.3), type="pe3")
cpara <- list(cop=PLcop, para=120, alpha=0.4, beta=0.05)
cpara <- list(cop=P, para=8, alpha=0.5, beta=0.05, reflect=2)

UVt <- simCOP(5000, cop=composite1COP, para=cpara, graphics=TRUE)


xout  <- seq(1, 6, by=0.5)
ffx   <- lmomco::plmomco(xout, xpara)
ffy   <- EvuCOP(ffx, cop=composite1COP, para=cpara)
yout  <- lmomco::qlmomco(ffy, ypara)


"EvuEMPIRCOP" <-
function(u, para=NULL, deluv=0.01, ff=seq(0.01, 0.99, by=0.01), ctype="weibull") {
  uv.grid   <- EMPIRgrid(para=para, deluv=deluv, ctype=ctype) # CPU extreme w/o weibull/hazen
  uv.inv1   <- EMPIRgridderinv(empgrid=uv.grid)
  init.para <- lmomco::vec2par(c(1.5, 2), type="kur")
  mus <- sapply(u, function(t) { qq <- NULL
           for(f in ff) {
             QR <- EMPIRqua.regress(f=f, empinv=uv.inv1)
             qq <- c(qq, approx(QR$U, QR$V, xout=t, rule=2)$y)
           }
           kur <- NULL
           suppressMessages(   try(kur <- lmomco::disfitqua(qq, ff, type="kur",
                                                           init.para=init.para), silent=TRUE) )
           if(is.null(kur)) {
             suppressMessages( try(kur <- lmomco::disfitqua(qq, ff, type="kur"), silent=TRUE) )
             if(is.null(kur)) return(NA)
           }
           if(length(kur) == 1) return(NA)
           lmomco::par2lmom(kur)$lambdas[1] })
  return(mus)
}

    tparf <- function(par) c(exp(par[1]), pnorm( par[2] ), pnorm( par[3] ))
    rparf <- function(par) c(log(par[1]), qnorm( par[2] ), qnorm( par[3] ))
    ofunc <- function(par, rhoS=NA, infSv=NA) { # objective function
      mypara <- tparf(par)
      mypara <- list(  cop=PLcop,         para=mypara[1], alpha=mypara[2], beta=mypara[3])
      rhoT   <- rhoCOP(cop=composite1COP, para=mypara)    # simulated Spearman Rho
      infTv  <- LzCOPpermsym(cop=composite1COP, para=mypara, n=permsynsim, type="halton", as.vec=TRUE)
      (rhoT - rhoS)^2 + mean( (infTv - infSv)^2 )
    }

nsam <- 40
nsim <- 50

ctype <- "weibull"
permsynsim <- 5E4

plot(qlmomco(UVt[,1], xpara), qlmomco(UVt[,2], ypara), pch=16, col=grey(0, 0.1))
lines(xout, yout, col="lightgreen", lwd=6)

DC <- matrix(nrow=nsim, ncol=length(xout))
DS <- matrix(nrow=nsim, ncol=length(xout))
DE <- matrix(nrow=nsim, ncol=length(xout))
for(i in seq_len(nsim)) {
  UV      <- rCOP(nsam, cop=composite1COP, para=cpara, resamv01=TRUE)
  rhoS    <- cor(UV[,1], UV[,2], method="spearman")
  infS  <- LzCOPpermsym(cop=EMPIRcop, para=UV, n=permsynsim, type="halton", as.vec=FALSE, ctype=ctype)
  infSv <- LzCOPpermsym(cop=EMPIRcop, para=UV, n=permsynsim, type="halton", as.vec=TRUE,  ctype=ctype)
  init.par <- rparf(c(1, 0.5, 0.5)); rt <- NULL # initial parameter guess
  try( rt <- optim(init.par, fn=ofunc, rhoS=rhoS, infSv=infSv) ) # 3D optimization
  uvpara <- tparf(rt$par)
  uvpara <- list(      cop=PLcop,         para=uvpara[1], alpha=uvpara[2], beta=uvpara[3])
  rhoT <- rhoCOP(      cop=composite1COP, para=uvpara)
  infT <- LzCOPpermsym(cop=composite1COP, para=uvpara, n=permsynsim, type="halton", as.vec=FALSE)

  Xs      <- lmomco::qlmomco(UV[,1], xpara)
  Ys      <- lmomco::qlmomco(UV[,2], ypara)
  ppxs    <- lmomco::pp(Xs, sort=FALSE)
  ppys    <- lmomco::pp(Ys, sort=FALSE)
  UVs     <- data.frame(U=ppxs, V=ppys)
  xparas  <- lmomco::lmom2par(lmomco::lmoms(Xs), type="pe3")
  yparas  <- lmomco::lmom2par(lmomco::lmoms(Ys), type="pe3")
  ffxs    <- lmomco::plmomco(xout, xparas)
  ffysc   <- EvuCOP(ffxs, cop=composite1COP, para=cpara )
  ffyss   <- EvuCOP(ffxs, cop=composite1COP, para=uvpara)
  ffyse   <- EvuEMPIRCOP(ffx, para=UV)
  youtsc  <- lmomco::qlmomco(ffysc, yparas)
  youtss  <- lmomco::qlmomco(ffyss, yparas)
  youtse  <- lmomco::qlmomco(ffyse, yparas)
  lines(xout, youtsc, col="blue",     lty=2, lwd=0.8)
  lines(xout, youtss, col="seagreen", lty=2, lwd=0.8)
  lines(xout, youtse, col="red",      lty=2, lwd=0.8)
  DC[i,] <- youtsc
  DS[i,] <- youtss
  DE[i,] <- youtse
}
lines(xout, yout, lwd=3)

lines(xout, yout, col="lightgreen", lwd=6)

lines(xout, colMeans(DC), col="blue",     lwd=3)
lines(xout, colMeans(DS), col="seagreen", lwd=3)
lines(xout, colMeans(DE), col="red",      lwd=3)



stop()





ffx <- 0.25
qua <- 0.60
cpara <- list(cop=PLcop, para=120, alpha=0.4, beta=0.05)

ff <- seq(0.001, 0.999, by=0.001)
t <- sapply(ff, function(v) derCOPinv(cop=composite1COP, u=ffx, t=v, para=cpara) )
plot(t, ff, xlim=c(0, 1), ylim=c(0, 1), type="l", col="blue",
     xlab="Quantile", ylab="Conditional distribution function, in nonexceedance probability")

deluv <- .Machine$double.eps^0.5
cs  <- (composite1COP(ffx+deluv, v=ff, para=cpara) -
        composite1COP(ffx-deluv, v=ff, para=cpara)) / (2*deluv)
kur <- disfitqua(cs, ff, type="kur")
k   <- qlmomco(ff, kur)
lines(ff, k, col="blue", lty=2)
points(approx(ff, t, xout=qua)$y, qua, pch=16, col="blue")
points(qua.regressCOP(f=qua, ffx, cop=composite1COP, para=cpara)$V,   qua, pch=0, col="blue", cex=1.5)
points(derCOPinv(cop=composite1COP, u=ffx, t=qua, para=cpara), qua, pch=5, col="blue", cex=2.0)
points(approx(k, ff, xout=qua)$y, qua, cex=1.2, pch=21, col="blue", bg="white", lwd=2)


UV <- simCOP(nsam, cop=composite1COP, para=cpara, graphics=FALSE)

i <- 3
for(myctype in c("checkerboard", "bernstein")) {
  i <- i - 1
  deluv <- 0.1 # huge fragility in the Delta for the partial derivative computation
  es <- (EMPIRcop(ffx+deluv, v=ff, para=UV, ctype=myctype) -
         EMPIRcop(ffx-deluv, v=ff, para=UV, ctype=myctype)) / (2*deluv)
  lines(ff, es, col="red", lty=i, type="b")
  kur <- lmomco::disfitqua(es, ff, type="kur", init.para=vec2par(c(1.5, 2), type="kur"))
  k   <- lmomco::qlmomco(ff, kur)
  lines(ff, k, col="red", lty=2)
  points(approx(k, ff, xout=qua)$y, qua, col="red", pch=1)
}

egrideluv <- 0.05
uv.grid <- EMPIRgrid(para=UV, deluv=egrideluv, ctype="checkerboard") # CPU extreme w/o weibull/hazen
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)

fff <- attr(uv.inv1, "rownames") #seq(egrideluv, 1-egrideluv, by=egrideluv)
qq <- NULL
for(f in fff) {
  QR <- EMPIRqua.regress(f=f, empinv=uv.inv1, ctype="checkerboard")
  qq <- c(qq, approx(QR$U, QR$V, xout=ffx)$y)
}
kur <- disfitqua(qq, fff, type="kur", init.para=vec2par(c(1.5, 2), type="kur"))
points(qlmomco(qua, kur), qua, pch=25, col="salmon4", bg="salmon1")

points(quantile(UV$V[UV$U > ffx-0.1 & UV$U < ffx+0.1], probs=qua), qua, pch=24)



cpara <- list(cop=CLcop, para=8, alpha=0.5, beta=0.05, reflect=2)

plot(c(0,1), c(0,1), type="n")
abline(v=ffx)
mu <- EvuCOP(u=ffx, cop=composite1COP, para=cpara)
nsim <- 100
bias <- NULL
for(i in seq_len(nsim)) {
  UV <- simCOP(nsam, cop=composite1COP, para=para, ploton=FALSE)

  uv.grid <- EMPIRgrid(para=UV, deluv=0.05, ctype="checkerboard") # CPU extreme w/o weibull/hazen
  uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)

  qq <- NULL
  for(f in attr(uv.inv1, "rownames")) {
    QR <- EMPIRqua.regress(f=f, empinv=uv.inv1, ctype="checkerboard")
    qq <- c(qq, approx(QR$U, QR$V, xout=ffx)$y)
  }
  kur <- disfitqua(qq, attr(uv.inv1, "rownames"), type="kur",
                     init.para=vec2par(c(1.5, 2), type="kur"))
  mus <- par2lmom(kur)$lambdas[1]
  abline(h=mu, col="blue")
  abline(h=mus, col="red", lty=2)
  bias <- c(bias, mus)
}

