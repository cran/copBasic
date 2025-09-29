\encoding{utf8}
\name{LCOMDIA_GHcop}
\alias{LCOMDIA_GHcop}
\title{L-comoment Ratio Diagrams for 2- and 3-Parameter Gumbel--Hougaard Extreme Value Copula}
\description{
L-comoment ratio diagrams by computation example are shown for the 2- and 3-parameter \emph{Gumbel--Hougaard copula} (\code{\link{GHcop}}).
}
\author{W.H. Asquith}
\seealso{\code{\link{lcomCOP}}
}
\examples{
\dontrun{
  # ------------------------------------------------------------------------------------
  plot(c(0, 1), c(-0.2, +0.3), type="n", xaxs="i", yaxs="i",
       xlab="Spearman Rho", ylab="L-coskew or L-cokurtosis")
  legend("topright", c("L-coskew of GHcop(para=c(b1,b2))",
                       "L-cokurtosis of GHcop(para=c(b1,b2))",
                       "L-coskew of GHcop(para=c(b1,p2,p3))",
                       "L-cokurtosis of GHcop(para=c(b1,p2,p3))"), bty="n",
        pch=c(15,16,3,4), pt.cex=0.5, col=c("seagreen4", "orchid3", "brown3", "blue3"))
  fileGH2 <- tempfile(); fileGH3 <- tempfile()
  nsim <- 10000
  for(i in seq_len(nsim)) {
    if(length(grep("000$", as.character(i)))) message(i,"-", appendLF=FALSE)
    b1 <- exp(runif(1, min=-6, max=5))+1 # used for both 2- and 3-parameter
    b2 <- ifelse(length(grep("[13579]$", as.character(i))),  # 1/2 between 0..1 and
                     exp(runif(1, min=0, max=5)), runif(1, min=0, max=1)) # 1/2 > 1
    t  <- b1; p2 <- runif(1, min=0, max=1); p3 <- runif(1, min=0, max=1)
    GH2 <- lcomCOP(cop=GHcop, para=c(b1, b2),     stop.on.error=FALSE)
    GH3 <- lcomCOP(cop=GHcop, para=c(b1, p2, p3), stop.on.error=FALSE)
    GH2$lcomUV <- round(GH2$lcomUV, digits=8)
    GH2$lcomVU <- round(GH2$lcomVU, digits=8)
    GH3$lcomUV <- round(GH3$lcomUV, digits=8)
    GH3$lcomVU <- round(GH3$lcomVU, digits=8)
    d2 <- data.frame(b1, b2,
            GH2T2.12=GH2$lcomUV[2], GH2T3.12=GH2$lcomUV[3], GH2T4.12=GH2$lcomUV[4],
            GH2T2.21=GH2$lcomVU[2], GH2T3.21=GH2$lcomVU[3], GH2T4.21=GH2$lcomVU[4])
    d3 <- data.frame(b1, p2, p3,
            GH3T2.12=GH3$lcomUV[2], GH3T3.12=GH3$lcomUV[3], GH3T4.12=GH3$lcomUV[4],
            GH3T2.21=GH3$lcomVU[2], GH3T3.21=GH3$lcomVU[3], GH3T4.21=GH3$lcomVU[4])
    row.names(d2) <- row.names(d3) <- NULL
    #print(d2); print(d3)
    if(abs(d2$GH2T2.12) < 1 & d2$GH2T2.12 > 0) {
      points(d2$GH2T2.12, d2$GH2T3.12, cex=0.5, pch=15, col="seagreen4")
      points(d2$GH2T2.12, d2$GH2T4.12, cex=0.5, pch=16, col="orchid3"  )
    }
    if(abs(d2$GH2T2.21) < 1 & d2$GH2T2.21 > 0) {
      points(d2$GH2T2.21, d2$GH2T3.21, cex=0.5, pch=15, col="seagreen4")
      points(d2$GH2T2.21, d2$GH2T4.21, cex=0.5, pch=16, col="orchid3"  )
    }
    if(abs(d3$GH3T2.12) < 1 & d3$GH3T2.12 > 0) {
      points(d3$GH3T2.12, d3$GH3T3.12, cex=0.5, pch= 3, col="brown3")
      points(d3$GH3T2.12, d3$GH3T4.12, cex=0.5, pch= 4, col="blue3" )
    }
    if(abs(d3$GH3T2.21) < 1 & d3$GH3T2.21 > 0) {
      points(d3$GH3T2.21, d3$GH3T3.21, cex=0.5, pch= 3, col="brown3")
      points(d3$GH3T2.21, d3$GH3T4.21, cex=0.5, pch= 4, col="blue3" )
    }
    k <- i == 1 # triggering mechanism for write.table()
    write.table(d2, file=fileGH2, sep="\t", append=! k, col.names=k, row.names=FALSE)
    write.table(d3, file=fileGH3, sep="\t", append=! k, col.names=k, row.names=FALSE)
  }
  message("done", appendLF=FALSE)
  fGH2 <- read.table(fileGH2, sep="\t", header=TRUE)
  fGH3 <- read.table(fileGH3, sep="\t", header=TRUE)
  # feather::write_feather(fGH2, "LCMrawGH2.feather")
  # feather::write_feather(fGH3, "LCMrawGH3.feather")
  # ------------------------------------------------------------------------------------}
}
\keyword{L-comoments (ratio diagram)}
\keyword{L-comoment ratio diagram}
