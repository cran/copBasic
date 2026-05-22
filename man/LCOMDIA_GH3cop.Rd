\encoding{utf8}
\name{LCOMDIA_GH3cop}
\alias{LCOMDIA_GH3cop}
\title{L-comoment Ratio Diagrams for 3-Parameter Gumbel--Hougaard Extreme Value Copula}
\description{
L-comoment ratio diagrams by computation example are shown for the 3-parameter \emph{Gumbel--Hougaard copula} (\code{\link{GHcop}}) with \emph{Marshall--Olkin copula} bounds (\code{\link{MOcop}}). Diagram computations for the 2-parameter version are found under \code{\link{LCOMDIA_GH2cop}}. Diagram computations for the 1-parameter version are found among many copulas in \code{\link{LCOMDIA_ManyCops}}.
}
\author{W.H. Asquith}
\seealso{\code{\link{lcomCOP}}, \code{\link{LCOMDIA_GH2cop}}, \code{\link{LCOMDIA_ManyCops}}
}
\examples{
\dontrun{
  # ------------------------------------------------------------------------------------
  # L-comoments of the GH3. Alert, there is a commented out write_feather below, so for
  # a "production" run, uncomment that. The reasoning for kicking-out output every nsim
  # so that another R session can already be "harvesting" output should the user seek
  # to study the output early. Later additions to these Examples are expected to hone
  # the results for polynomial boundary and polygonal domain of the relations.
  PLOT <- TRUE
  for(f in 1:20) {
    if(PLOT) {
      plot(c(0, 1), c(-0.2, +0.3), type="n", xaxs="i", yaxs="i", las=1,
           xlab="Spearman Rho", ylab="L-coskew (cT3) or L-cokurtosis (cT4)")
      legend("topright", c(    "L-coskew of GHcop(para=c(b1, p2, p3))",
                           "L-cokurtosis of GHcop(para=c(b1, p2, p3))"), bty="n",
            pch=c(3,4), pt.cex=0.5, col=c("seagreen4", "orchid3"))
    }
    fileGH3 <- tempfile(); message("Temp File = ", fileGH3)
    nsim <- 10000
    for(i in seq_len(nsim)) {
      if(length(grep("000$", as.character(i)))) message(i,"-", appendLF=FALSE)
      b1  <- 10^(runif(1, min=-5, max=3))+1
      p2  <- runif(1, min=0, max=1); p3 <- runif(1, min=0, max=1)
      GH3 <- lcomCOP(cop=GHcop, para=c(b1, p2, p3),       stop.on.error=FALSE)
      d3  <- data.frame(b1, p2, p3,
               T2.12=GH3$lcomUV[2], T3.12=GH3$lcomUV[3], T4.12=GH3$lcomUV[4],
               T2.21=GH3$lcomVU[2], T3.21=GH3$lcomVU[3], T4.21=GH3$lcomVU[4])
      row.names(d3) <- NULL; #print(d3)
      if(abs(d3$T2.12) < 1 & d3$T2.12 > 0) {
        if(PLOT) points(d3$T2.12, d3$T3.12, cex=0.5, pch=15, col="seagreen4")
        if(PLOT) points(d3$T2.12, d3$T4.12, cex=0.5, pch=16, col="orchid3"  )
      }
      if(abs(d3$T2.21) < 1 & d3$T2.21 > 0) {
        if(PLOT) points(d3$T2.21, d3$T3.21, cex=0.5, pch=15, col="seagreen4")
        if(PLOT) points(d3$T2.21, d3$T4.21, cex=0.5, pch=16, col="orchid3"  )
      }
      k <- i == 1 # triggering mechanism for write.table()
      write.table(d3, file=fileGH3, sep="\t", append=!k, col.names=k, row.names=FALSE)
    }
    message("done")
    fGH3 <- read.table(fileGH3, sep="\t", header=TRUE)
    file <- paste0("LCM_GH3_", f, ".feather")
    message("writing file '", file, "' to ", getwd())
    # feather::write_feather(fGH3, file)
  }
  # ------------------------------------------------------------------------------------
  files <- list.files(pattern=".feather")
  files <- files[grep("GH3_", files)]
  GH3 <- NULL
  for(file in files) GH3 <- rbind(GH3, as.data.frame(feather::read_feather(file)))
  # ------------------------------------------------------------------------------------
  plot(GH3$T2.12, GH3$T3.12, cex=0.2, pch=16, col=grey(0.3), las=1,
       xlim=c(0, 0.10), ylim=c(-0.07, 0.0))
  abline(0, -0.33) # }
}
\keyword{L-comoments (ratio diagram)}
\keyword{L-comoment ratio diagram}
