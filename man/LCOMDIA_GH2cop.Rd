\encoding{utf8}
\name{LCOMDIA_GH2cop}
\alias{LCOMDIA_GH2cop}
\title{L-comoment Ratio Diagrams for 2-Parameter Gumbel--Hougaard Extreme Value Copula}
\description{
L-comoment ratio diagrams by computation example are shown for the 2-parameter \emph{Gumbel--Hougaard copula} (\code{\link{GHcop}}). Diagram computations for the 3-parameter version are found under \code{\link{LCOMDIA_GH3cop}}. Diagram computations for the 1-parameter version are found among many copulas in \code{\link{LCOMDIA_ManyCops}}.
}
\author{W.H. Asquith}
\seealso{\code{\link{lcomCOP}}, \code{\link{LCOMDIA_GH3cop}}, \code{\link{LCOMDIA_ManyCops}}
}
\examples{
\dontrun{
  # b1 <- 1 # fixing 2pGHcop gives it L-coskews and L-cokurtoses matching the Clayton.
  GH   <- lcomCOP(cop=GHcop, para=c(1, 1.23107734))
  clfunc  <- function(k, rho=NA) rho - rhoCOP(cop=CLcop, para=exp(k) - 1)
  cl   <- exp( uniroot(clfunc, c(-9, +5), rho=GH$lcomUV[2])$root)- 1
  CL   <- lcomCOP(cop=CLcop, para=cl) # }

\dontrun{
  # ------------------------------------------------------------------------------------
  # L-comoments of the GH2. Alert, there is a commented out write_feather below, so for
  # a "production" run, uncomment that. The reasoning for kicking-out output every nsim
  # so that another R session can already be "harvesting" output should the user seek
  # to study the output earlier than total run will make.
  PLOT <- FALSE
  for(f in 1:20) {
    if(PLOT) {
      plot(c(0, 1), c(-0.2, +0.2), type="n", xaxs="i", yaxs="i", las=1,
           xlab="Spearman Rho", ylab="L-coskew (cT3) or L-cokurtosis (cT4)")
      legend("topright", c("L-coskew of GHcop(para=c(b1,b2))",
                           "L-cokurtosis of GHcop(para=c(b1,b2))"), bty="n",
            pch=c(15, 16), pt.cex=0.5, col=c("seagreen4", "orchid3"))
    }
    fileGH2 <- tempfile(); message("Temp File = ", fileGH2)
    nsim <- 10000
    for(i in seq_len(nsim)) {
      if(length(grep("000$", as.character(i)))) message(i,"-", appendLF=FALSE)
      b1 <- 10^(runif(1, min=-5, max=3))+1
      b2 <- ifelse(length(grep("[13579]$", as.character(i))),   # 1/2 between 0..1
              10^runif(1, min=0, max=3), 10^runif(1, min=-6, max=0)) # and 1/2 > 1
      GH2 <- lcomCOP(cop=GHcop, para=c(b1, b2), stop.on.error=FALSE)
      GH2$lcomUV <- round(GH2$lcomUV, digits=12)
      GH2$lcomVU <- round(GH2$lcomVU, digits=12)
      d2 <- data.frame(b1, b2,
              T2.12=GH2$lcomUV[2], T3.12=GH2$lcomUV[3], T4.12=GH2$lcomUV[4],
              T2.21=GH2$lcomVU[2], T3.21=GH2$lcomVU[3], T4.21=GH2$lcomVU[4])
      row.names(d2) <- NULL; #print(d2)
      if(abs(d2$T2.12) < 1 & d2$T2.12 > 0) {
        if(PLOT) points(d2$T2.12, d2$T3.12, cex=0.5, pch=15, col="seagreen4")
        if(PLOT) points(d2$T2.12, d2$T4.12, cex=0.5, pch=16, col="orchid3"  )
      }
      if(abs(d2$T2.21) < 1 & d2$T2.21 > 0) {
        if(PLOT) points(d2$T2.21, d2$T3.21, cex=0.5, pch=15, col="seagreen4")
        if(PLOT) points(d2$T2.21, d2$T4.21, cex=0.5, pch=16, col="orchid3"  )
      }
      k <- i == 1 # triggering mechanism for write.table()
      write.table(d2, file=fileGH2, sep="\t", append=! k, col.names=k, row.names=FALSE)
    }
    message("done")
    fGH2 <- read.table(fileGH2, sep="\t", header=TRUE)
    file <- paste0("LCM_GH2_", f, ".feather")
    message("writing file '", file, "' to ", getwd())
    # feather::write_feather(fGH2, file)
  }
  # ------------------------------------------------------------------------------------
  files <- list.files(pattern=".feather")
  files <- files[grep("GH2_", files)]
  GH2 <- NULL
  for(file in files) GH2 <- rbind(GH2, as.data.frame(feather::read_feather(file)))

  dr <- 0.001; rhos <- seq(0,1, by=dr); T2 <- c(GH2$T2.12, GH2$T2.21)
  T3 <- c(GH2$T3.12, GH2$T3.21);        T4 <- c(GH2$T4.12, GH2$T4.21)
  T2 <- c(0,0, T2, 1,1); T3 <- c(0,0, T3, 0,0); T4 <- c(0,0, T4, 0,0)
  df <- data.frame(T2, T3, T4)
  df <- df[df$T2 >= 0,] # giant simulations show no situations of negatives
  df$T2[df$T2 > 1] <- 1 # giant simulations show only a few meet this condition and are
  # greater than one at the fifth or beyond decimal, so declare as round-off issues and
  # not failure itself in the 2-parameter GHcop. (Like 3 in 200,000 simulations)
  df <- df[! (df$T2 > 0.997 & df$T3 > -0.468319*df$T2 + 0.468319), ]

  plot(df$T2, df$T3, pch=16, cex=0.2, col=grey(0.8), las=1,
       xlab="Spearman Rho", ylab="L-coskew (cT3)")
  negT3 <- posT3 <- NULL
  for(rho in rhos) {
    negT3 <- c(negT3, min(df$T3[abs(df$T2 - rho) <= dr]))
    posT3 <- c(posT3, max(df$T3[abs(df$T2 - rho) <= dr]))
  }
  X <- rhos[is.finite(negT3)]; Y <- negT3[is.finite(negT3)]
  negT3 <- lm(Y~I(X^ 1)+I(X^ 2)+I(X^ 3)+I(X^ 4)+I(X^ 5)+I(X^ 6)+
                I(X^ 7)+I(X^ 8)+I(X^ 9)+I(X^10)+I(X^11)+I(X^12) )
  X <- rhos[is.finite(posT3)]; Y <- posT3[is.finite(posT3)]
  posT3 <- lm(Y~I(X^ 1)+I(X^ 2)+I(X^ 3)+I(X^ 4)+I(X^ 5)+I(X^ 6)+
                I(X^ 7)+I(X^ 8)+I(X^ 9)+I(X^10)+I(X^11)+I(X^12) )
  lines(rhos, predict(negT3, newdata=data.frame(X=rhos)), lwd=2)
  lines(rhos, predict(posT3, newdata=data.frame(X=rhos)), lwd=2)

  t2t3hull <- grDevices::chull(df$T2, df$T3)
  polygon(df$T2[t2t3hull],  df$T3[t2t3hull], col=NULL, border="seagreen4", lwd=3,lty=2)
  write.table(data.frame(T2=df$T2[t2t3hull], T3=df$T3[t2t3hull]),
              file="lcom_GH2cop_nodes.txt", sep="\t")
  # The user might consider plotting this polygon() underneath L-comoment curves
  # computed and drawn through the recipe in LCOMDIA_ManyCops.Rd.

  X <- df$T2[t2t3hull]; Y <- df$T3[t2t3hull]
  X <- c(X[Y < 0], 0, 1); Y <- c(Y[Y < 0], 0, 0)
  negT3 <- lm(Y~I(X^ 1)+I(X^ 2)+I(X^ 3)+I(X^ 4)+I(X^ 5)+I(X^ 6)+
                I(X^ 7)+I(X^ 8)+I(X^ 9)+I(X^10)+I(X^11)+I(X^12) )
  X <- df$T2[t2t3hull]; Y <- df$T3[t2t3hull]
  X <- c(X[Y > 0], 0, 1); Y <- c(Y[Y > 0], 0, 0)
  posT3 <- lm(Y~I(X^ 1)+I(X^ 2)+I(X^ 3)+I(X^ 4)+I(X^ 5)+I(X^ 6)+
                I(X^ 7)+I(X^ 8)+I(X^ 9)+I(X^10)+I(X^11)+I(X^12) )
  plot(df$T2, df$T3, pch=16, cex=0.2, col="lightgreen", las=1,
       xlab="Spearman Rho", ylab="L-coskew (cT3)")
  lines(rhos, predict(negT3, newdata=data.frame(X=rhos)), lwd=2)
  lines(rhos, predict(posT3, newdata=data.frame(X=rhos)), lwd=2)
  print(negT3, 12); print(posT3, 12)
  "T3fT2" <- function(rho, type=c("neg", "pos")) {
    type <- match.arg(type)
    if(type == "neg") {
      cs <- c(1.77467318015e-04,
          -3.65537522263e-01, 1.32139171758e+00, -2.26891136600e+01, 2.14717293829e+02,
          -1.20819018654e+03, 4.36103722683e+03, -1.04460882313e+04, 1.67876630052e+04,
          -1.78950352179e+04, 1.21363232163e+04, -4.74068383919e+03, 8.11989541291e+02)
    } else if(type == "pos") {
      cs <- c(-8.77010579087e-06,
           2.45271423943e-01, -2.67994483238e-01, 1.28094964505e+00, -1.28531448606e+01,
           7.61335214074e+01, -2.87746627019e+02, 7.17887377219e+02, -1.19681860869e+03,
           1.31930086424e+03, -9.23025213380e+02, 3.71244634445e+02, -6.53809357075e+01)
    } else {
      stop("should not be here in logic")
    }
    z <- sapply(rho, function(r) sum(sapply(seq_len(length(cs)),
                                     function(p) cs[p] * r^(p-1))))
    return(z)
  }
  plot(df$T2, df$T3, pch=16, cex=0.2, col="lightgreen", las=1,
       xlab="Spearman Rho", ylab="L-coskew (cT3)")
  lines(rhos, T3fT2(rhos, type='neg')) # a last check that the polynomial approximations
  lines(rhos, T3fT2(rhos, type='pos')) # have been ported to subroutine acceptably.
  # The user might consider plotting these two polynomials underneath L-comoment curves
  # computed and drawn through the recipe in LCOMDIA_ManyCops.Rd.
  # ------------------------------------------------------------------------------------}
}
\keyword{L-comoments (ratio diagram)}
\keyword{L-comoment ratio diagram}
