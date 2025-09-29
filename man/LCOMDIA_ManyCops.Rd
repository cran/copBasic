\encoding{utf8}
\name{LCOMDIA_ManyCops}
\alias{LCOMDIA_ManyCops}
\title{L-comoment Ratio Diagrams for Many Copulas}
\description{
L-comoment ratio diagrams by computation example are shown for many copulas of the \pkg{copBasic} package.
}
\author{W.H. Asquith}
\seealso{\code{\link{lcomCOP}}, \code{\link{LCOMDIA_GH2cop}}, \code{\link{LCOMDIA_GH3cop}}, \code{\link{ORDSUMcop}}
}
\examples{
\dontrun{
  # Demonstration of an L-comoment ratio diagram as function of Spearman Rho for various
  # copula where each copula is fit to the Spearman Rho and the L-comoments computed.
  # ------------------------------------------------------------------------------------
  # The copulas here are permutation symmetric, which means that T*.12 and T*.21 are
  # identical, but plotting for the T*.21 are shown here to make it easier for the user
  # to insert permutation asymmetry say by blending in the breveCOP() or other
  # composition frameworks. L-cokurtosis is negated to have it simply plot away from
  # the L-coskew.
  # ------------------------------------------------------------------------------------
  LCMempty <- list(lcomUV=rep(NA, 5), lcomVU=rep(NA, 5)) # for comprehensive show of
  # features and handling for situations of a Spearman Rho being incompatible with a
  # copula, which is the case for the N4212cop that we have reversed here to put the
  # tail dependency for moderate Spearman Rho into the upper tail.
  rN4212cop <- function(u,v, para, ...) COP(u,v, cop=N4212cop, para=para, reflect=2)

  clfunc  <- function(k, rho=NA) rho - rhoCOP(cop=COP,
                           para=list(reflect=2, cop=CLcop,  para=exp(k)-1))
  gefunc  <- function(k, rho=NA) rho - rhoCOP(cop=gEVcop,    para=pnorm(k))
  ghfunc  <- function(k, rho=NA) rho - rhoCOP(cop=GHcop,     para=exp(k)+1)
  glfunc  <- function(k, rho=NA) rho - rhoCOP(cop=GLcop,     para=exp(k)  )
  hrfunc  <- function(k, rho=NA) rho - rhoCOP(cop=HRcop,     para=exp(k)  )
  jofunc  <- function(k, rho=NA) rho - rhoCOP(cop=JOcopB5,   para=exp(k)+1)
  mofunc  <- function(k, rho=NA) rho - rhoCOP(cop=MOcop,     para=pnorm(rep(k,2)))
  n12func <- function(k, rho=NA) rho - rhoCOP(cop=rN4212cop, para=exp(k)+1)
  n20func <- function(k, rho=NA) rho - rhoCOP(cop=rN4220cop, para=exp(k)  )
  rafunc  <- function(k, rho=NA) rho - rhoCOP(cop=RAYcop,    para=pnorm(k))
  rffunc  <- function(k, rho=NA) rho - rhoCOP(cop=COP,
                           para=list(reflect=2, cop=RFcop,  para=pnorm(k)))
  D <- NULL; rs <- c(0.0001, 0.0002, 0.0005, 0.0008, 0.001, 0.0013, 0.002, 0.003,
                     0.004, 0.005, 0.006, 0.007, 0.008, 0.009, 0.01, 0.015,
                     seq(0.02, 0.99, by=0.005), 0.99,
              0.991, 0.992, 0.993, 0.994, 0.995, 0.996, 0.997, 0.998)
  rs <- sort(unique(rs)) # to fix defects
  for(r in rs) {  message(r, "-", appendLF=FALSE) # Spearman Rhos,
    ga   <- 2*sin(pi*r/6)
    GA   <- lcomCOP(cop=NORMcop, para=ga)
    cl   <- exp(  uniroot(clfunc,  c(-9,+5), rho=r)$root)-1
    CL   <- lcomCOP(cop=COP, para=list(cop=CLcop, para=cl, reflect=2))
    fr   <- FRcop(rhotau=r, cortype="rho")$para
    FR   <- lcomCOP(cop=FRcop, para=fr)
    ge   <- pnorm(uniroot(gefunc,  c(-4,+4), rho=r)$root)
    GE   <- lcomCOP(cop=gEVcop, para=ge)
    gh   <- exp(  uniroot(ghfunc,  c(-10,+4), rho=r)$root)+1
    GH   <- lcomCOP(cop=GHcop, para=gh)
    gl   <- exp(  uniroot(glfunc,  c(-9,+4), rho=r)$root)
    GL   <- lcomCOP(cop=GLcop, para=gl)
    hr   <- exp(  uniroot(hrfunc,  c(-9,+4), rho=r)$root)
    HR   <- lcomCOP(cop=HRcop, para=hr)
    jo   <- exp(  uniroot(jofunc,  c(-10,+4), rho=r)$root)+1
    JO   <- lcomCOP(cop=JOcopB5, para=jo)
    mo   <- pnorm( uniroot(mofunc, c(-6,+6), rho=r)$root)
    mo   <- rep(mo, 2)
    MO   <- lcomCOP(cop=MOcop, para=mo)
    if(r > 0.4784176) { # rhoCOP(rN4212cop, para=1) = 0.4784176
      n12 <- exp(  uniroot(n12func, c(-9,+4), rho=r)$root)+1
      N12  <- lcomCOP(cop=rN4220cop, para=n12)
    } else {
      n12 <- NA; N12 <- LCMempty
    }
    n20  <- exp(  uniroot(n20func, c(-10,+4), rho=r)$root)
    N20  <- lcomCOP(cop=rN4220cop, para=n20)
    ra   <- RAYcop(rho=r)
    RA   <- lcomCOP(cop=RAYcop, para=ra)
    rf   <- pnorm(  uniroot(rffunc, c(-6,6), rho=r)$root)
    RF   <- lcomCOP(cop=COP, para=list(cop=RFcop, para=rf, reflect=2))
    TF   <- lcomCOP(cop=Tcop, para=c(ga,1))
    x <- data.frame(rho=r, CLpara=cl, FRpara=fr, GApara=ga, GEpara=ge, GHpara=gh,
                GLpara=gl, HRpara=hr, JOpara=jo, MOpara=mo[1],
                N12para=n12, N20para=n20, RApara=ra, RFpara=rf, TFpara=ga,
                 CLT2.12=CL$lcomUV[2],   CLT2.21=CL$lcomVU[2],
                 CLT3.12=CL$lcomUV[3],   CLT3.21=CL$lcomVU[3],
                 CLT4.12=CL$lcomUV[4],   CLT4.21=CL$lcomVU[4],
                 FRT2.12=FR$lcomUV[2],   FRT2.21=FR$lcomVU[2],
                 FRT3.12=FR$lcomUV[3],   FRT3.21=FR$lcomVU[3],
                 FRT4.12=FR$lcomUV[4],   FRT4.21=FR$lcomVU[4],
                 GAT2.12=GA$lcomUV[2],   GAT2.21=GA$lcomVU[2],
                 GAT3.12=GA$lcomUV[3],   GAT3.21=GA$lcomVU[3],
                 GAT4.12=GA$lcomUV[4],   GAT4.21=GA$lcomVU[4],
                 GET2.12=GE$lcomUV[2],   GET2.21=GE$lcomVU[2],
                 GET3.12=GE$lcomUV[3],   GET3.21=GE$lcomVU[3],
                 GET4.12=GE$lcomUV[4],   GET4.21=GE$lcomVU[4],
                 GHT2.12=GH$lcomUV[2],   GHT2.21=GH$lcomVU[2],
                 GHT3.12=GH$lcomUV[3],   GHT3.21=GH$lcomVU[3],
                 GHT4.12=GH$lcomUV[4],   GHT4.21=GH$lcomVU[4],
                 GLT2.12=GL$lcomUV[2],   GLT2.21=GL$lcomVU[2],
                 GLT3.12=GL$lcomUV[3],   GLT3.21=GL$lcomVU[3],
                 GLT4.12=GL$lcomUV[4],   GLT4.21=GL$lcomVU[4],
                 HRT2.12=HR$lcomUV[2],   HRT2.21=HR$lcomVU[2],
                 HRT3.12=HR$lcomUV[3],   HRT3.21=HR$lcomVU[3],
                 HRT4.12=HR$lcomUV[4],   HRT4.21=HR$lcomVU[4],
                 JOT2.12=JO$lcomUV[2],   JOT2.21=JO$lcomVU[2],
                 JOT3.12=JO$lcomUV[3],   JOT3.21=JO$lcomVU[3],
                 JOT4.12=JO$lcomUV[4],   JOT4.21=JO$lcomVU[4],
                 MOT2.12=MO$lcomUV[2],   MOT2.21=MO$lcomVU[2],
                 MOT3.12=MO$lcomUV[3],   MOT3.21=MO$lcomVU[3],
                 MOT4.12=MO$lcomUV[4],   MOT4.21=MO$lcomVU[4],
               N12T2.12=N12$lcomUV[2], N12T2.21=N12$lcomVU[2],
               N12T3.12=N12$lcomUV[3], N12T3.21=N12$lcomVU[3],
               N12T4.12=N12$lcomUV[4], N12T4.21=N12$lcomVU[4],
               N20T2.12=N20$lcomUV[2], N20T2.21=N20$lcomVU[2],
               N20T3.12=N20$lcomUV[3], N20T3.21=N20$lcomVU[3],
               N20T4.12=N20$lcomUV[4], N20T4.21=N20$lcomVU[4],
                 RAT2.12=RA$lcomUV[2],   RAT2.21=RA$lcomVU[2],
                 RAT3.12=RA$lcomUV[3],   RAT3.21=RA$lcomVU[3],
                 RAT4.12=RA$lcomUV[4],   RAT4.21=RA$lcomVU[4],
                 RFT2.12=RF$lcomUV[2],   RFT2.21=RF$lcomVU[2],
                 RFT3.12=RF$lcomUV[3],   RFT3.21=RF$lcomVU[3],
                 RFT4.12=RF$lcomUV[4],   RFT4.21=RF$lcomVU[4],
                 TFT2.12=TF$lcomUV[2],   TFT2.21=TF$lcomVU[2],
                 TFT3.12=TF$lcomUV[3],   TFT3.21=TF$lcomVU[3],
                 TFT4.12=TF$lcomUV[4],   TFT4.21=TF$lcomVU[4] )
    D <- rbind(D, x)
  }
  Z <- D[1,]
  Z$rho <- 0
  Z[,grep("[12]$", names(Z))] <- rep(0, length(grep("[12]$", names(Z))))
  Z[,grep("para$", names(Z))] <- floor(     Z[,grep("para$", names(Z))])
  D <- rbind(Z, D)
  Z <- D[1,]
  Z$rho <- 1
  Z[,grep("[12]$", names(Z))] <- rep(0,   length(grep("[12]$", names(Z))))
  Z[,grep("para$", names(Z))] <- rep(Inf, length(grep("para$", names(Z))))
  D <- rbind(   D, Z)
  message("done")
  row.names(D) <- NULL
  D$MOT3.12[is.na(D$MOT3.12)] <- D$MOT3.21[is.na(D$MOT3.12)]
  D$TFT3.12[      is.na(D$TFT3.12)] <- approx(D$rho[! is.na(D$TFT3.12)],
      D$TFT3.12[! is.na(D$TFT3.12)],     xout=D$rho[  is.na(D$TFT3.12)])$y
  # write.table(D, file="LCOMDIA_ManyCops.txt", sep=",", row.names=FALSE)
  # ------------------------------------------------------------------------------------
  ix <- c(9, 6, 3, 12, 10, 5, 11, 1, 14, 2, 7, 8, 4, 13)
  cols <- hcl.colors(length(ix), palette="Dark3")
  cols <- cols[ix]
  coll <- cols[unlist( sapply(seq_len(length(ix)), function(i) list(rep(i, 2))) )]
  rs <- D$rho
  plot(rs, D$GHT3.12, ylim=c(-0.20, 0.20), type="n", las=1,
      xlab="Spearman Rho", ylab="L-coskew (cT3) or -negated L-cokurtosis (cT4)")
  lines(rs, D$CLT3.12,  col=cols[ 1]); lines(rs, -D$CLT4.12,  col=cols[ 1], lty=2)
  lines(rs, D$CLT3.21,  col=cols[ 1]); lines(rs, -D$CLT4.21,  col=cols[ 1], lty=2)
  lines(rs, D$FRT3.12,  col=cols[ 2]); lines(rs, -D$FRT4.12,  col=cols[ 2], lty=4)
  lines(rs, D$FRT3.21,  col=cols[ 2]); lines(rs, -D$FRT4.21,  col=cols[ 2], lty=4)
  lines(rs, D$GAT3.12,  col=cols[ 3]); lines(rs, -D$GAT4.12,  col=cols[ 3], lty=2)
  lines(rs, D$GAT3.21,  col=cols[ 3]); lines(rs, -D$GAT4.21,  col=cols[ 3], lty=2)
  lines(rs, D$GET3.12,  col=cols[ 4]); lines(rs, -D$GET4.12,  col=cols[ 4], lty=4)
  lines(rs, D$GET3.21,  col=cols[ 4]); lines(rs, -D$GET4.21,  col=cols[ 4], lty=4)
  lines(rs, D$GHT3.12,  col=cols[ 5]); lines(rs, -D$GHT4.12,  col=cols[ 5], lty=2)
  lines(rs, D$GHT3.21,  col=cols[ 5]); lines(rs, -D$GHT4.21,  col=cols[ 5], lty=2)
  lines(rs, D$GLT3.12,  col=cols[ 6]); lines(rs, -D$GLT4.12,  col=cols[ 6], lty=4)
  lines(rs, D$GLT3.21,  col=cols[ 6]); lines(rs, -D$GLT4.21,  col=cols[ 6], lty=4)
  lines(rs, D$HRT3.12,  col=cols[ 7]); lines(rs, -D$HRT4.12,  col=cols[ 7], lty=2)
  lines(rs, D$HRT3.21,  col=cols[ 7]); lines(rs, -D$HRT4.21,  col=cols[ 7], lty=2)
  lines(rs, D$JOT3.12,  col=cols[ 8]); lines(rs, -D$JOT4.12,  col=cols[ 8], lty=4)
  lines(rs, D$JOT3.21,  col=cols[ 8]); lines(rs, -D$JOT4.21,  col=cols[ 8], lty=4)
  lines(rs, D$MOT3.21,  col=cols[ 9]); lines(rs, -D$MOT4.21,  col=cols[ 9], lty=2)
  lines(rs, D$MOT3.12,  col=cols[ 9]); lines(rs, -D$MOT4.12,  col=cols[ 9], lty=2)
  lines(rs, D$N12T3.12, col=cols[10]); lines(rs, -D$N12T4.12, col=cols[10], lty=4)
  lines(rs, D$N12T3.21, col=cols[10]); lines(rs, -D$N12T4.21, col=cols[10], lty=4)
  lines(rs, D$N20T3.12, col=cols[11]); lines(rs, -D$N20T4.12, col=cols[11], lty=2)
  lines(rs, D$N20T3.21, col=cols[11]); lines(rs, -D$N20T4.21, col=cols[11], lty=2)
  lines(rs, D$RAT3.12,  col=cols[12]); lines(rs, -D$RAT4.12,  col=cols[12], lty=4)
  lines(rs, D$RAT3.21,  col=cols[12]); lines(rs, -D$RAT4.21,  col=cols[12], lty=4)
  lines(rs, D$RFT3.12,  col=cols[13]); lines(rs, -D$RFT4.12,  col=cols[13], lty=2)
  lines(rs, D$RFT3.21,  col=cols[13]); lines(rs, -D$RFT4.21,  col=cols[13], lty=2)
  lines(rs, D$TFT3.12,  col=cols[14]); lines(rs, -D$TFT4.12,  col=cols[14], lty=4)
  lines(rs, D$TFT3.21,  col=cols[14]); lines(rs, -D$TFT4.21,  col=cols[14], lty=4)
  txt <- c("cT3 CLcop(reflect)",        "-cT4 CLcop(reflect)",
           "cT3 FRcop (equals zero)",   "-cT4 FRcop",
           "cT3 NORMcop (equals zero)", "-cT4 NORMcop",
           "cT3 gEVcop",                "-cT4 gEVcop",
           "cT3 GHcop",                 "-cT4 GHcop",
           "cT3 GLcop",                 "-cT4 GLcop",
           "cT3 HRcop",                 "-cT4 HRcop",
           "cT3 JOcopB5",               "-cT4 JOcopB5",
           "cT3 MOcop (a=b)",           "-cT4 MOcop (a=b)",
           "cT3 rN4212cop",             "-cT4 rN4212cop",
           "cT3 rN4220cop",             "-cT4 rN4220cop",
           "cT3 RAYcop",                "-cT4 RAYcop",
           "cT3 RFcop",                 "-cT4 RFcop",
           "cT3 Tcop (equals zero)",    "-cT4 Tcop")
  legend("bottomleft", txt, bty="n", cex=0.7, ncol=4, lty=c(1,2,1,4), col=coll)
  mtext("L-comoment ratio diagram for various copula", line=0.8)
  # ------------------------------------------------------------------------------------
  # Now, simulation, note that in samples T[2+].[12|21] are not perfectly equal even for
  # known permutation asymmetry. These could be average or used in isolation. Here, we
  # just plot the two separately. So, there are double the data points. It takes a lot
  # of samples for single sample to look authoritative.
  T2 <- T3 <- T4 <- NULL
  for(i in 1:60) {
    uv  <- simCOP(2000, cop=GLcop, para=0.95, graphics=FALSE)
    lcm <- lmomco::lcomoms2(uv, nmom=4); srho <- (lcm$T2[1,2] + lcm$T2[2,1]) / 2
    points(rep(srho, 2),  c(lcm$T3[1,2], lcm$T3[2,1]), cex=0.6, lwd=0.8, pch=24)
    points(rep(srho, 2), -c(lcm$T4[1,2], lcm$T4[2,1]), cex=0.6, lwd=0.8, pch=25)
    T2 <- c(T2, srho)
    T3 <- c(T3, mean(c(lcm$T3[1,2], lcm$T3[2,1])))
    T4 <- c(T4, mean(c(lcm$T4[1,2], lcm$T4[2,1])))
  } # BTW : compare the srho to srho <- cor(UV, method="spearman")
  mT2 <- mean(T2); mT3 <- mean(T3); mT4 <- mean(T4)
  points(mT2,  mT3, col=cols[6], lwd=2, bg=grey(0.9), pch=24, cex=2)
  points(mT2, -mT4, col=cols[6], lwd=2, bg=grey(0.9), pch=25, cex=2)
  # ------------------------------------------------------------------------------------
  # L-comoment ratio diagram of L-coskew and L-cokurtosis.
  # Again both 12 and 21 (UV and VU) are plotted but for permutation symmetry these
  # "with respect to's" have the same L-comoments. The GLcop was used in the simulation;
  # so, the lines for it are thicker to see that the general centrality of the sample
  # L-comoments provide evidence back to the GLcop copula.
  plot(D$GHT3.12, xlim=c(0, 0.20), ylim=c(-0.075, 0.075), type="n", las=1,
       xlab="L-coskew (cT3)", ylab="L-cokurtosis (cT4)")
  lines( D$CLT3.12,  D$CLT4.12, col=cols[ 1])
  lines( D$CLT3.21,  D$CLT4.21, col=cols[ 1], lty=2)
  lines( D$FRT3.12,  D$FRT4.12, col=cols[ 2])
  lines( D$FRT3.21,  D$FRT4.21, col=cols[ 2], lty=4)
  lines( D$GAT3.12,  D$GAT4.12, col=cols[ 3])
  lines( D$GAT3.21,  D$GAT4.21, col=cols[ 3], lty=2)
  lines( D$GET3.12,  D$GET4.12, col=cols[ 4])
  lines( D$GET3.21,  D$GET4.21, col=cols[ 4], lty=4)
  lines( D$GHT3.12,  D$GHT4.12, col=cols[ 5])
  lines( D$GHT3.21,  D$GHT4.21, col=cols[ 5], lty=2)
  lines( D$GLT3.12,  D$GLT4.12, col=cols[ 6],        lwd=2)
  lines( D$GLT3.21,  D$GLT4.21, col=cols[ 6], lty=4, lwd=2)
  lines( D$HRT3.12,  D$HRT4.12, col=cols[ 7])
  lines( D$HRT3.21,  D$HRT4.21, col=cols[ 7], lty=2)
  lines( D$JOT3.12,  D$JOT4.12, col=cols[ 8])
  lines( D$JOT3.21,  D$JOT4.21, col=cols[ 8], lty=4)
  lines( D$MOT3.12,  D$MOT4.12, col=cols[ 9])
  lines( D$MOT3.21,  D$MOT4.21, col=cols[ 9], lty=2)
  lines(D$N12T3.12, D$N12T4.12, col=cols[10])
  lines(D$N12T3.21, D$N12T4.21, col=cols[10], lty=2)
  lines(D$N20T3.12, D$N20T4.12, col=cols[11])
  lines(D$N20T3.21, D$N20T4.21, col=cols[11], lty=2)
  lines( D$RAT3.12,  D$RAT4.12, col=cols[12])
  lines( D$RAT3.21,  D$RAT4.21, col=cols[12], lty=4)
  lines( D$RFT3.12,  D$RFT4.12, col=cols[13])
  lines( D$RFT3.21,  D$RFT4.21, col=cols[13], lty=2)
  lines( D$TFT3.12,  D$TFT4.12, col=cols[14])
  lines( D$TFT3.21,  D$TFT4.21, col=cols[14], lty=4)
  points( T3,  T4, lwd=0.8, cex=0.8)
  points(mT3, mT4, lwd=2,   cex=2, pch=21, col=cols[6], bg=grey(0.9))
  txt <- c("CLcop(reflect)[UV]", "CLcop(reflect)[VU]",
             "FRcop[UV]",         "FRcop[VU]",
           "NORMcop[UV]",       "NORMcop[VU]",
            "gEVcop[UV]",        "gEVcop[VU]",
             "GHcop[UV]",         "GHcop[VU]",
             "GLcop[UV]",         "GLcop[VU]",
             "HRcop[UV]",         "HRcop[VU]",
             "JOcop[UV]",         "JOcop[VU]",
             "MOcop[UV] (a=b)",   "MOcop[VU] (a=b)",
           "rN4212cop[UV]",   "rN4212cop[VU]",
           "rN4220cop[UV]",   "rN4220cop[VU]",
             "RAcop[UV]",         "RAcop[VU]",
             "RFcop[UV]",         "RFcop[VU]",
              "Tcop[UV]",          "Tcop[VU]")
   legend("bottomright", txt, cex=0.7, ncol=4, lty=c(1,2), col=coll)
   mtext("L-comoment ratio diagram for various copula", line=0.8)
   # ------------------------------------------------------------------------------------}
}
\keyword{L-comoments (ratio diagram)}
\keyword{L-comoment ratio diagram}
