library(lmomco)
library(quantreg)

lo <- .Machine$double.eps; hi <- 1 - lo
xpara <- lmomco::vec2par(c(3, 0.6, +0), type="pe3")
ypara <- lmomco::vec2par(c(3, 0.4, -0), type="pe3")

ff    <- c(0.00001, 0.0001, 0.001, seq(0.01, 0.99, by=0.01), 0.999, 0.9999, 0.99999)
cpara <- list(cop=COP, para=list(cop=CLcop, para=8), reflect=3, alpha=0, beta=0.3)
UVt <- simCOP(200, cop=composite1COP, para=cpara, graphics=FALSE, resamv01=TRUE)
xsam <- qlmomco(UVt[,1], xpara)
ysam <- qlmomco(UVt[,2], ypara)
xsap <- lmom2par(lmoms(xsam), type="pe3")
ysap <- lmom2par(lmoms(ysam), type="pe3")


xthe  <- qlmomco(ff, xpara)
xldc  <- qlmomco(ff, xsap)

if(sign(cor(xsam, ysam, method="spearman")) < 0) {
  ythe  <- qlmomco(1-ff, ypara)
  yldc  <- qlmomco(1-ff, ysap )
} else {
  ythe  <- qlmomco(  ff, ypara)
  yldc  <- qlmomco(  ff, ysap )
}

xout  <- qlmomco(ff, xsap)
yout  <- qlmomco(ff, ysap)
ffx   <- plmomco(xout, xsap)
ffy   <- plmomco(yout, ysap)
ffyu  <- EvuCOP(ffx, cop=composite1COP, para=cpara, delu=.Machine$double.eps^0.25)
ffxv  <- EuvCOP(ffy, cop=composite1COP, para=cpara, delv=.Machine$double.eps^0.25)
mmyu  <- med.regressCOP( ffx, cop=composite1COP, para=cpara)
mmxv  <- med.regressCOP2(ffy, cop=composite1COP, para=cpara)
youtu <- qlmomco(ffyu, ysap)
xoutv <- qlmomco(ffxv, xsap)
ymmtu <- qlmomco(mmyu$V, ysap)
xmmtv <- qlmomco(mmxv$U, xsap)

plot(xthe, ythe, type="l", lwd=3)
points(xsam, ysam, pch=16, col=grey(0, 0.1))
#points(qlmomco(pp(xsam, sort=FALSE, a=0), xsap),
#       qlmomco(pp(ysam, sort=FALSE, a=0), ysap), pch=16, col=grey(0, 0.5))



lines(xout,  youtu, col="palegreen1", lwd=2, lty=1, type="b")
lines(xoutv, yout,  col="palegreen3", lwd=2, lty=1, type="b")
lines(xout,  ymmtu, col="purple1",    lwd=2, lty=2)
lines(xmmtv, yout,  col="purple4",    lwd=2, lty=2)


lines(xldc, yldc, col="cyan")

print(sqrt(mean((xout-xthe)^2 + (yout-ythe)^2)))

stop()

medss <- quantreg::rqss(ysam~qss(xsam, constraint="D"), tau=0.5)
lines(sort(xsam), predict(medss, newdata=data.frame(xsam=sort(xsam))), col="orange", lwd=2)
medss <- quantreg::rqss(xsam~qss(ysam, constraint="D"), tau=0.5)
lines(sort(ysam), predict(medss, newdata=data.frame(ysam=sort(ysam))), col="orange", lwd=2)


gamss <- mgcv::gam(y~s(x), gamma=1)
lines(sort(x), predict(gamss, newdata=data.frame(x=sort(x))), col="blue")

gamss <- mgcv::gam(y~s(x), gamma=2)
lines(sort(x), predict(gamss, newdata=data.frame(x=sort(x))), col="blue", lty=2)

gamss <- mgcv::gam(y~s(x), gamma=3)
lines(sort(x), predict(gamss, newdata=data.frame(x=sort(x))), col="blue", lty=3)


zz <- bicoploc(x,y, xout=xout, xpara=xpara, ypara=ypara, plotuv=FALSE, plotxy=FALSE)
 lines(zz$organic$xout, zz$organic$bicoploc_emp, col="red", lwd=2)
 lines(zz$organic$xout, zz$organic$bicoploc, col="red", lwd=2, lty=2)
 lines(zz$organic$xout, zz$organic$locpair, col="lightblue", lwd=2, lty=2)

