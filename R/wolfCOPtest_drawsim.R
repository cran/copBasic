"wolfCOPtest_drawsim" <-
function(swolf, add=FALSE, alpha=0.05, pdffile=NA,
                title=NA, ulab="Us", vlab="Vs",
                mai=c(0.75, 0.75, 0.50, 0.25), ...) {
  if(! exists("table", swolf)) {
    warning("swolf does not have its table of simulations, consider running wolfCOPtest, which\n",
            "means that aslist argument somehow needs to acquire true")
    return(NULL)
  }
  df <- swolf$table

  if(ncol(df) != 6) {
    warning("swolf$table expected as 6 columns, try add.cor.tests=TRUE in wolfCOPtest")
    return(NULL)
  }

  hyphen <- "\u00ad"

  if(! is.na(pdffile)) pdf(pdffile)
    alt <- ""
    if(nrow(df) > 1) {
      alt <- paste0(' by ties or given "zmat" requiring simulation (m=',
                    prettyNum(nrow(df), big.mark=","), ')')
    }
    mix <- seq(0.2, 0.8, by=0.2); oix <- seq(0.1, 0.9, by=0.2); lix <- c(0,1)
    tix <- sprintf("%0.2f", seq(0.05, 0.95, by=0.05)) # bottom, left, top, right
    tix <- tix[-grep("[1-9]0$", tix)]
    if(! add) {
      par(mai=mai, mgp=c(2.5, 0.5, 0), lend=1, ljoin=1, las=1, xpd=NA)
      plot(  df$sigmas, df$sigmas_pvl, type="n", xlim=lix, ylim=lix, xaxs="i", yaxs="i", las=1,
           xaxt="n", yaxt="n", bty="n", xlab="", ylab="", bty="n" )
      par(las=0)
      mtext(paste0("P", hyphen, "value of respective statistic"), 2, line=2)
      par(las=1)
      mtext(paste0("Schweizer-Wolff Sigma", alt), 1, line=2)
      axis(1, at=lix, labels=TRUE,  lwd=0, lwd.ticks=0)
      axis(2, at=lix, labels=TRUE,  lwd=0, lwd.ticks=0)
      axis(1, at=mix, labels=TRUE,  lwd=0, lwd.ticks=1, tcl=+0.60)
      axis(2, at=mix, labels=TRUE,  lwd=0, lwd.ticks=1, tcl=+0.60)
      axis(3, at=mix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.60)
      axis(4, at=mix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.60)
      axis(1, at=oix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.60)
      axis(2, at=oix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.60)
      axis(3, at=oix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.60)
      axis(4, at=oix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.60)
      axis(1, at=tix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.25)
      axis(2, at=tix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.25)
      axis(3, at=tix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.25)
      axis(4, at=tix, labels=FALSE, lwd=0, lwd.ticks=1, tcl=+0.25)
      px <- c(    par()$usr[1:2],   rev(par()$usr[1:2]),  par()$usr[1])
      py <- c(rep(par()$usr[3], 2), rep(par()$usr[4], 2), par()$usr[3])
      par(lend=2); polygon(px, py, border=1, col=NA); par(lend=1)
    }
    if(exists("kendall_taus_pvl", df) & exists("spearman_rhos_pvl", df)) {
      cf <- rbind(data.frame(x=df$sigmas, y=df$kendall_taus_pvl,  pch=6, col="deepskyblue3"),
                  data.frame(x=df$sigmas, y=df$spearman_rhos_pvl, pch=2, col="salmon3"     ))
      cf <- cf[sample(seq_len(nrow(cf)), nrow(cf), replace=FALSE),]
      points(cf$x, cf$y, lwd=0.85, pch=cf$pch, cex=0.8, col=cf$col)
    } else {
      warning("need swolf$table to have p-values Kendall Tau and Spearman Rho, skipping their\n",
              "drawing but will leave in the legend(), rerun wolfCOPtest(add.cor.tests=TRUE)")
    }
    points(median(df$sigmas), median(df$sigmas_pvl), pch=21, cex=1.8, lwd=1.6,
                                                             col="darkgreen",   bg="grey95"     )
    points(mean(  df$sigmas), mean(  df$sigmas_pvl), pch=23, cex=0.9, lwd=1.6,
                                                             col="darkorchid4", bg="darkorchid1")
    par(ljoin=0, lend=1); lines( df$sigmas, df$sigmas_pvl,  lwd=1.2, col="grey20"); par(ljoin=1)

    if(alpha >= 1) alpha <- 1
    if(alpha <= 0) alpha <- 0
    fmt <- abs(floor(log10(alpha)))
    suppressWarnings( fmt <- ifelse(fmt == Inf, "0", sprintf(paste0("%0.", fmt, "f"), alpha)) )
    # In sprintf(fmt, alpha) : one argument not used by format '0'

    txt <- c(paste0("Statistical significance level alpha = ", fmt),
             paste0("Line connecting p", hyphen, "values of ordered Sigmas"),
             paste0(  "Mean p", hyphen, "value of Sigmas"),
             paste0("Median p", hyphen, "value of Sigmas"),
             paste0("P", hyphen, "values of Kendall Taus by ordered Sigmas"),
             paste0("P", hyphen, "values for Spearman Rhos by ordered Sigmas"))
    pch    <- c(NA, NA, 23, 21, 6, 2)
    lty    <- c(1.0, 4.0, NA, NA, NA, NA)
    lwd    <- c(1.2, 1.2, NA, NA, NA, NA)
    col    <- c("grey40", "grey20", "darkorchid4", "darkgreen", "deepskyblue3", "salmon3")
    pt.cex <- c(NA, NA, 0.9, 1.8, 0.8, 0.8)
    pt.bg  <- c(NA, NA, "darkorchid1", "grey95", NA, NA)
    pt.lwd <- c(NA, NA, 1.6, 1.6, 0.85, 0.85)

    if(0 < as.numeric(fmt) & as.numeric(fmt) < 1) {
      lines(par()$usr[1:2], rep(alpha, 2), lty=4, lwd=1.2, col="grey40")
    } else {
      txt <- txt[-1]
      pch <- pch[-1]; lty <- lty[-1]; lwd <- lwd[-1]; col <- col[-1]
      pt.cex <- pt.cex[-1]; pt.bg <- pt.bg[-1]; pt.lwd <- pt.lwd[-1]
    }

    legend("topright", txt, bty="o", inset=0.02, box.lty=NULL, box.col=NA, bg=NA, cex=0.7,
                       y.intersp=1.1, pch=pch, lty=lty, lwd=lwd, col=col,
                       pt.cex=pt.cex, pt.bg=pt.bg, pt.lwd=pt.lwd, ...)

    if(! is.na(title)) mtext(title, side=3, line=0.8)
    if(! (is.na(ulab) & is.na(ulab))) {
      txt <- paste0(swolf$sample_size, " sample size, ",
                    swolf$num_uuniq, " unique ", ulab, ", and ",
                    swolf$num_vuniq, " unique ", vlab)
      mtext(txt, side=3, line=0.05, cex=0.8)
    }

  if(! is.na(pdffile)) dev.off()
}
