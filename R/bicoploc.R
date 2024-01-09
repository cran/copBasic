"bicoploc" <-
function(xp, yp=NULL, xout=NA, xpara=NULL, ypara=NULL, dtypex="nor", dtypey="nor",
         ctype=c("weibull", "hazen", "bernstein", "checkerboard"), kumaraswamy=TRUE,
         plotuv=TRUE, plotxy=TRUE, adduv=FALSE, addxy=FALSE, snv=FALSE, limout=TRUE,
         autoleg=TRUE, xleg="topleft", yleg=NULL, rugxy=TRUE, ruglwd=0.5,
         xlim=NULL, ylim=NULL, titleuv="", titlexy="", titlecex=1,
         a=0, ff=pnorm(seq(-5, +5, by=0.1)), locdigits=6,
         paracop=TRUE, verbose=TRUE, x=NULL, y=NULL, ...) {

  USE_PARA_COP <- paracop # The capital letters become easier to "find" in the function
  rm(paracop)

  lo <- .Machine$double.eps; hi <- 1 - lo
  # being at and close to the edges of probability helps to ensure that we by default stress the
  # algorithm in how we handle or trap issues on the edges.
  ff <- sort( unique( c(0, lo, lo^0.5, ff, 1-lo^0.5, hi, 1) ) ) # assurance policy
  # also stretching to the edges, ensures too that we get simply stress various subsystems

  if(is.matrix(xp) | is.data.frame(xp)) {
    xp <- xp[,1]; yp <- yp[,2]
  }

  if(any(is.na(xp))) {
    message("some of the xp are NA, not fully configured to handle such circumstances")
    return(NULL)
  }
  if(any(is.na(yp))) {
    message("some of the yp are NA, not fully configured to handle such circumstances")
    return(NULL)
  }
  if(length(xp) != length(yp)) {
    message("length of xp and yp are not identical")
    return(NULL)
  }
  n <- length(xp)

  if(is.null(xout)) xout <- NA

  if(plotuv) adduv <- FALSE
  if(plotxy) addxy <- FALSE

  PPtxt <- "plotting position"
  a  <- a[1] # silent devectorization
  aa <- as.character(round(as.numeric(a), digits=4)) # to fit into built-in lmomco
  aa <- switch(aa, "0"      = "Weibull",
                   "0.3175" = "Median",
                   "0.375"  = "Blom",
                   "0.40"   = "Cunnane",
                   "0.44"   = "Gringorten",
                   "0.5"    = "Hazen")
  if(! is.null(aa)) PPtxt  <- paste0(aa, " ", PPtxt)

  init.kur <- c(1.5, 2) # initial guesses at Kumaraswamy parameters, these were somewhat guessed at
  # but seem have robustness. Maybe better could be found.

  ctype    <- match.arg(      ctype     )
  txt      <- unlist(strsplit(ctype, ""))
  ctyTXT   <- paste0(toupper(txt[1]), paste(txt[2:length(txt)], sep="", collapse="")) # extract first letter and upper case it
  ctyTXTuv <- paste0("Diagonal inversion points in (U,V) domain strictly by ", ctyTXT, " empirical copula")
  ctyTXTxy <- paste0("Diagonal inversion points in (X,Y) domain strictly by ", ctyTXT, " empirical copula")
  PCHcty   <- 4   # a times symbol
  PCHcex   <- 1.2 # size of the letter for plotting
  PCHlwd   <- 1.8
  PCHcol   <- "mediumorchid1"        # symbol plotting color for the empirical copula diagonal
  PCHbg    <- "white"  # background symbol plotting color for the empirical copula diagonal
  COPpch   <- 23
  COPbg    <- "chocolate1"
  COPcol   <- "chocolate1"
  COPptlwd <- 0.8
  COPcex   <- 0.7
  COPlty   <- 1
  COPlwd   <- 6
  DIAeol   <- "darkgreen"  # color for plotting of kumaraswamy line smooth of diagonal
  DIAcol   <- "seagreen"  # color for plotting of kumaraswamy line smooth of diagonal
  DIAbg    <- "lightgreen"
  DIAcex   <- 0.91
  DIApch   <- 21       # background symbol plotting color for the diagonal
  DIAlwd   <- 3
  DIAlty   <- 1
  DIAptlwd <- 1.1
  OUTcex   <- 1.3
  OUTcol   <- "darkred"    #            symbol plotting color for the Xout, if given
  OUTbg    <- "white"  # background symbol plotting color for the Xout, if given
  OUTpch   <- 16       # plotting character for the predictions from the inverse copula diagonal
  OUTptlwd <- 1
  LOCpch   <- 22       # plotting character for line of organic correlation predictions
  DATcol   <- "black"  # symbol plotting color for the Xs
  DATbg    <- "white"  # background symbol plotting color for the Xs
  DATcex   <- 1.1
  DATpch   <- 21
  DATptlwd <- 0.9
  LOClwd   <- 3
  LOClty   <- 2
  LOCcol   <- "blue"
  LOCbg    <- "white"
  LOCcex   <- 1.9
  LOCptlwd <- 1
  LOCUlty  <- 2
  LOCUlwd  <- 1.5
  LOCUcol  <- "deepskyblue2"
  LEGcex   <- 0.75    # legend() text expansion factor
  SEGlen   <- 3.5      # width of line segments in the legend()
  LEGinset <- 0.02
  RUGcol   <- "wheat3" # ticking color for rug() plotting

  FFedge  <- c(0.001, 0.999) # reasonably wide range when plotting marginal distributions, these
  qFFedge <- qnorm(FFedge)   # standard normal variates of reasonably deep in the tails
  qff     <- qnorm(ff)       # standard normal variates of the ff vector
  # remember, the ff vector is the nonexceedance probability of the diagonal of the copula and hence
  # is that joint probability for which we are playing with.

  i <- 0 # a counter only for the verbose messaging

  if(verbose) message("(", i <- i + 1, ") plotting positions for U and V, recall lmomco::pp() ties.method=first")
  u  <- lmomco::pp(xp, a=a, sort=FALSE) # plotting positions
  v  <- lmomco::pp(yp, a=a, sort=FALSE) # plotting positions
  UV <- data.frame(U=u, V=v) # to become the para argument for the empirical copula

  sortX <- sort(xp) # making other diagnostic plots to potentially come easier to assemble

  # parametric marginal distribution of X and Y (if parameter estimation is otherwise required)
  if(is.null(xpara)) {
    if(verbose) message("(", i <- i + 1, ") starting parameter estimation for X parametric ",
                                         "margin by L-moments for dtypex")
    xpara <- NULL
    if(is.null(x)) {
      thex <- xp
    } else {
      if(verbose) message("(", i <- i + 1, ") using the alternative X values in argument x ",
                                           "for parameter estimation")
      thex <- x
    }
    lmr <- lmomco::lmoms(thex[is.finite(thex)], no.stop=TRUE)
    if(is.null(lmr)) {
      message("degenerate sample in variable 'thex', L-moments in X direction seem to have failed")
      return(NULL)
    }
    try(xpara <- lmomco::lmom2par(lmr, type=dtypex), silent=TRUE)
    if(is.null(xpara)) {
      message("parameter estimation for X by method of L-moments has failed, try another dtypex?")
      return(NULL)
    }
    if(is.null(dtypex)) {
      if(exists("type", xpara)) dtypex <- xpara$type
    }
  } else {
    if(is.null(x)) { # secondary look at the x is for support of the rug() call and nothing more
      thex <- xp
    } else {
      thex <- x
    }
    if(is.null(dtypex) || is.na(dtypex)) {
      dtypex <- "--" # reset to empty to ensure that what was given is used from xpara forevermore
    }
  }

  if(is.null(ypara)) {
    if(verbose) message("(", i <- i + 1, ") starting parameter estimation for Y parametric ",
                                         "margin by L-moments for dtypex")
    ypara <- NULL
    if(is.null(y)) {
      they <- yp
    } else {
      if(verbose) message("(", i <- i + 1, ") using the alternative X values in argument x ",
                                           "for parameter estimation")
      they <- y
    }
    lmr <- lmomco::lmoms(they[is.finite(they)], no.stop=TRUE)
    if(is.null(lmr)) {
      message("degenerate sample in variable 'they', L-moments in Y direction seem to have failed")
      return(NULL)
    }
    try(ypara <- lmomco::lmom2par(lmomco::lmoms(they[is.finite(they)]), type=dtypey), silent=TRUE)
    if(is.null(ypara)) {
      message("parameter estimation for Y by method of L-moments has failed, try another dtypey?")
      print(they)
      return(NULL)
    }
    if(is.null(dtypey)) {
      if(exists("type", ypara)) dtypey <- ypara$type
    }
  } else { # this secondary look at the y is for support of the rug() call and nothing more
    if(is.null(y)) {
      they <- yp
    } else {
      they <- y
    }
    if(is.null(dtypey) || is.na(dtypey)) {
      dtypey <- "--" # reset to empty to ensure that what was given is used from ypara forevermore
    }
  }

  n_thex <- length( thex[is.finite(thex)] )
  n_they <- length( they[is.finite(they)] )

  if(verbose) message("(", i <- i + 1, ") determing association sign by Spearman Rho")
  # recall for the usual line of organic correlation that the sign of Rho sets the sign of the
  # slope of the line, it appears that we can do the same as part of the extension of the logic
  # with copulas to support negatively associated data sets
  rhoS    <- cor(xp, yp, method="spearman")
  rhosign <- sign( rhoS ) # kind of like "Wormsign"

  if(verbose) message("(", i <- i + 1, ") computing line of organic correlation by lmomco::lmrloc()")
  XYp <- data.frame(X=xp, Y=yp) # a bet permissive to allow NAs incoming but works with the is.finite
  XYp <- XYp[complete.cases(XYp),] # check above before the L-moment parameter estimation is called
  XYp <- XYp[is.finite(XYp[,1]) & is.finite(XYp[,2]),] # an finite check here means that we have the
  locpair  <- lmomco::lmrloc( XYp )  # locpair existing, -Inf and +Inf incoming likely stem from
  # simulation play in highly skewed distributions and not in the practical application on
  # real-world data compute the line of organic correlation (reduced major axis), using the
  # method of L-moments.
  names(locpair$loc_lmr) <- c("LMR_PAIR_Intercept", "LMR_PAIR_Slope")
  names(locpair$loc_pmr) <- c("PMR_PAIR_Intercept", "PMR_PAIR_Slope")

  xlmr <- lmomco::par2lmom(xpara)$lambdas; # print(xlmr)
  ylmr <- lmomco::par2lmom(ypara)$lambdas; # print(ylmr)
  m_para  <- rhosign * (ylmr[2] /           xlmr[2]) # slope by relative standard deviations
  b_para  <-            ylmr[1] - (m_para * xlmr[1]) # intercept to pass through the means
  locpara <- c(b_para, m_para)
  names(locpara) <- c("LMR_PARA_Intercept", "LMR_PARA_Slope")
  locpara <- list(loc_lmr=locpara)

  if(any(! is.na(xout))) {
    if(verbose) message("(", i <- i + 1, ") estimating xout by line of organic correlation")
    Yloc      <- locpair$loc_lmr[2] * sortX + locpair$loc_lmr[1]
    Yloc_out  <- locpair$loc_lmr[2] * xout  + locpair$loc_lmr[1]
    Ylocu     <- locpara$loc_lmr[2] * sortX + locpara$loc_lmr[1]
    Ylocu_out <- locpara$loc_lmr[2] * xout  + locpara$loc_lmr[1]

    names(Yloc)      <- NULL
    names(Yloc_out)  <- NULL
    names(Ylocu)     <- NULL
    names(Ylocu_out) <- NULL
  }

  # Invert the diagonals of copula. The ff is the joint probability Pr[X <= x & Y <= y] = C(u,v)
  # and we want to solve on the diagonal for U=V=t --> C(t,t) = ff on the supposition that the
  # diagonal of the copula is the mathematical structure useful for the problem at hand
  if(verbose) message("(", i <- i + 1, ") computing vector of primary diagonal of empirical copula")
  ec <- eco <- COP(ff,   ff, cop=EMPIRcop, para=UV, ctype=ctype, ...)
  # Nonexceedances of the U and V and a copy forming the "original empirical", we will optionally
  # smooth the ec (empirical copula) but never the eco (empirical copula original)

  # Here are two other ways to think about this diagonal and ultimately its inverse. Seems just hit
  # it at mass as in above COP() call is sufficient with fine enough resolution for approx()-like
  # calls to come or for setup of the Kumaraswamy [0,1]-bounded quantile function smooth estimation
  # to ensure that we are on the real number line.
  #ed <- diagCOP(cop=EMPIRcop, para=UV, ctype=ctype, ploton=FALSE, lines=FALSE, delt=0.001)
  #ed$t <- c(0, ed$t, 1); ed$diagcop <- c(0, ed$diagcop, 1)
  #ff <- ed$t; ec <- eco <- ed$diagcop
  #ec <- eco <- diagCOPatf(ff, cop=EMPIRcop, para=UV, ctype=ctype)
  if(kumaraswamy) { # https://en.wikipedia.org/wiki/Kumaraswamy_distribution
    if(verbose) message("(", i <- i + 1, ") fitting Kumaraswamy (method of percentiles)")
    kur.init.para   <- lmomco::vec2par(init.kur, type="kur")
        eckurpara   <- NULL
    try(  eckurpara <- lmomco::disfitqua(ec, ff, type="kur", init.para=kur.init.para), silent=TRUE)
    if(is.null(eckurpara)) {
      try(eckurpara <- lmomco::disfitqua(ec, ff, type="kur"),                          silent=TRUE)
    }
    if(is.null(eckurpara)) {
      if(verbose) message("(", i <- i + 1, ") checking Kumaraswamy, disfitqua failed, disabling")
      kumaraswamy <- FALSE # disabling
    } else if( length(unique(lmomco::par2qua(ff, eckurpara, paracheck=FALSE))) == 1 ) {
      if(verbose) message("(", i <- i + 1, ") checking Kumaraswamy, flat lined, disabling")
      kumaraswamy <- FALSE # disabling
    } else {
      ec <- lmomco::par2qua(ff, eckurpara, paracheck=FALSE) # the replacement of the "smoothed
      # diagonal of the empirical copula through the use of the Kumaraswamy distribution fit to
      # RMSE of method of percentiles. The ec are the "u=v=t" in general copula nomenclature.
    }
  }


  infS <- NA # only to have it populated on output
  ### BEGIN PARAMETRIC COPULA BLOCK
  if(USE_PARA_COP) {
    permsynsim <- 5E4
    infS  <- LzCOPpermsym(cop=EMPIRcop, para=UV, n=permsynsim, type="halton", as.vec=FALSE, ctype=ctype)
    infSv <- LzCOPpermsym(cop=EMPIRcop, para=UV, n=permsynsim, type="halton", as.vec=TRUE,  ctype=ctype)
    tparf <- function(par) c(exp(par[1]), pnorm( par[2] ), pnorm( par[3] ))
    rparf <- function(par) c(log(par[1]), qnorm( par[2] ), qnorm( par[3] ))
    ofunc <- function(par) { # objective function
      mypara <- tparf(par)
      mypara <- list(  cop=PLcop,         para=mypara[1], alpha=mypara[2], beta=mypara[3])
      rhoT   <- rhoCOP(cop=composite1COP, para=mypara)    # simulated Spearman Rho
      infTv  <- LzCOPpermsym(cop=composite1COP, para=mypara, n=permsynsim, type="halton", as.vec=TRUE)
      (rhoT - rhoS)^2 + mean( (infTv - infSv)^2 )
    }
    if(verbose) message("(", i <- i + 1, ") calling optim() for multiD optimization of parametric copula")
    init.par <- rparf(c(1, 0.5, 0.5)); rt <- NULL # initial parameter guess
    try( rt <- optim(init.par, fn=ofunc) ) # 3D optimization
    if(is.null(rt)) {
      message("composite1COP parameter estimation returned NULL, disabling parameteric copula")
      USE_PARA_COP <- FALSE
    } else {
      para.cop <- tparf(rt$par)
      para.cop <- list(    cop=PLcop,         para=para.cop[1], alpha=para.cop[2], beta=para.cop[3])
      rhoT <- rhoCOP(      cop=composite1COP, para=para.cop)
      infT <- LzCOPpermsym(cop=composite1COP, para=para.cop, n=permsynsim, type="halton", as.vec=FALSE)

      # JK <- simCOP(1000, cop=composite1COP, para=para.cop) # developer use only for intermediate check
      faqscop <- c(rhoS, infS, rhoT, infT, para.cop$alpha, para.cop$beta, para.cop$para)
      ctxt <- paste0("CopulaParameter", seq_len(length(para.cop$para)))
      faqscop <- round(faqscop, digits=6)
      names(faqscop) <- c("SpearmanRhoSample",       "LzCOPpermsymSample",
                          "SpearmanRhoFittedCopula", "LzCOPpermsymFittedCopula",
                          "Alpha", "Beta", ctxt)
      dtt <- COP(ff, ff, cop=composite1COP, para=para.cop, ...)
    }
  } ### END PARAMETRIC COPULA BLOCK

  if(! USE_PARA_COP) {
    dtt <- dtty <- rep(NA, length(ff)) # assurance of no misuse by NA these vectors should they leak
    faqscop <- c(rhoS, infS)
    faqscop <- round(faqscop, digits=6)
    names(faqscop) <- c("SpearmanRhoSample", "LzCOPpermsymSample")
  }

  if(rhosign < 0) { # negative association
    ecy  <- 1 - ec
    ecoy <- 1 - eco
    dtty <- 1 - dtt
  } else {          # positive association
    ecy  <-     ec
    ecoy <-     eco
    dtty <-     dtt
  }

  # An open question, do we 1-v the plotting of the diagonal on the Y-axis when Rho < 0
  # and also a 1 - lmomco::quakur() also, or does all of this reflect things in the wrong way?
  if(plotuv) {
    if(! adduv) {
      if(snv) {
        if(verbose) message("(", i <- i + 1, ") plotting the (U,V) domain in SNV units")
        xylim <- qFFedge
        plot(qff, qnorm(eco), xlim=xylim, ylim=xylim, type="n", las=1, xaxs="i", yaxs="i",
             xlab="SNV OF COPULA DIAGONAL (Vector of 'f' joint probability)", mgp=c(2.2, 0.75, 0),
             ylab="SNV INVERSE COPULA DIAGONAL (f = C(U,V))\nTHE U = V NONEXCEEDANCE PROBABILITY")
      } else {
        if(verbose) message("(", i <- i + 1, ") plotting the (U,V) domain in probability")
        xylim <- c(0, 1)
        plot(ff,       eco,  xlim=xylim, ylim=xylim, type="n", las=1, xaxs="r", yaxs="r",
             xlab="COPULA DIAGONAL (Vector of 'f' joint probability)", mgp=c(2.2, 0.75, 0),
             ylab="INVERSE COPULA DIAGONAL (f = C(U,V))\nTHE U = V NONEXCEEDANCE PROBABILITY")
      }
      mtext(titleuv, side=3, line=1, font=2, cex=titlecex)
      axis(3, axTicks(1), labels=FALSE, lwd=NA, lwd.ticks=1)
      axis(4, axTicks(2), labels=FALSE, lwd=NA, lwd.ticks=1)
    }
    if(snv) {
      if(USE_PARA_COP) lines(qff, qnorm(dtt), col=COPcol, lwd=COPlwd)
      points(    qff,     qnorm(eco),  pch=PCHcty,   cex=PCHcex, col=PCHcol, lwd=PCHlwd)
      points(qnorm(UV$U), qnorm(UV$V), pch=DATpch,   cex=DATcex, col=DATcol, lwd=DATptlwd, bg=DATbg)
      if(kumaraswamy)  lines(qff, qnorm(lmomco::qlmomco(ff, eckurpara)), col=DIAcol, lwd=DIAlwd, lend=2)
      points(    qff,     qnorm(ecy),  pch=DIApch,   cex=DIAcex, col=DIAeol, lwd=DIAptlwd, bg=DIAbg)
    } else {
      if(USE_PARA_COP) lines( ff, dtt, col=COPcol, lwd=COPlwd)
      points(     ff,           eco,   pch=PCHcty,   cex=PCHcex, col=PCHcol, lwd=PCHlwd)
      points(UV$U,      UV$V,          pch=DATpch,   cex=DATcex, col=DATcol, lwd=DATptlwd, bg=DATbg)
      if(kumaraswamy)  lines( ff, lmomco::qlmomco(ff, eckurpara),col=DIAcol, lwd=DIAlwd, lend=2)
      points(     ff,           ec,    pch=DIApch,   cex=DIAcex, col=DIAeol, lwd=DIAptlwd, bg=DIAbg)
    }
    if(autoleg) {
      lwd    <- c(COPlwd,  DIAlwd,        NA,      NA,        NA,  NA)
      lty    <- c(COPlty,  DIAlty,        NA,      NA,        NA,  NA)
      col    <- c(COPcol,  DIAcol,    DIAcol,  PCHcol,    DATcol,  NA)
      pch    <- c(    NA,      NA,    DIApch,  PCHcty,    DATpch,  NA)
      pt.bg  <- c(    NA,      NA,     DIAbg,  PCHbg,      DATbg,  NA)
      pt.lwd <- c(    NA,      NA,  DIAptlwd,  PCHlwd,  DATptlwd,  NA)
      pt.cex <- c(    NA,      NA,    DIAcex,  PCHcex,    DATcex,  NA)
      txt    <- c("Diagonal inverse of parametric asymmetric Plackett copula",
                  "Kumaraswamy smooth for diagonal inverse of the empirical copula coordinates",
                  "Coordinates of empirical copula diagonal inverse from joint probability vector",
                   ctyTXTuv,
       paste0("Paired data points (n=", prettyNum(n, big.mark=","), ") between U and V by ", PPtxt),
              'Note, interpretation of "best fit" to data points on this plot is not applicable.')
      if(! USE_PARA_COP) {
        nix <- -1
        lwd    <- lwd[   nix]
        lty    <- lty[   nix]
        col    <- col[   nix]
        pch    <- pch[   nix]
        pt.bg  <- pt.bg[ nix]
        pt.lwd <- pt.lwd[nix]
        pt.cex <- pt.cex[nix]
        txt    <- txt[   nix]
      }
      if(! kumaraswamy) {
        nix <- -grep("Kumaraswamy", txt)
        lwd    <- lwd[   nix]
        lty    <- lty[   nix]
        col    <- col[   nix]
        pch    <- pch[   nix]
        pt.bg  <- pt.bg[ nix]
        pt.lwd <- pt.lwd[nix]
        pt.cex <- pt.cex[nix]
        txt    <- txt[   nix]
      }
      opts <- par(no.readonly=TRUE)
      par(lend=2)
      legend(xleg, yleg, txt,  bty="o", cex=LEGcex, inset=LEGinset, seg.len=SEGlen, box.col=NA,
          bg="white", lwd=lwd, lty=lty, col=col, pch=pch, pt.bg=pt.bg, pt.lwd=pt.lwd, pt.cex=pt.cex)
      par(opts)
    }
  }

  if(verbose) message("(", i <- i + 1, ") solving for Y predictions along the diagonal inversion")
  # solve for Y, like missing record estimation using the best available information on the Y
  # distribution's parameters (i.e., the MOMENTS)
  # Back us off from -Inf and +Inf to NaN results when a probability = 0 | 1, to ensure numerical
    tmf <- ecy;  tmf[tmf <= lo] <- lo; tmf[tmf >= hi] <- hi
    Ysec0  <- lmomco::par2qua(tmf,  ypara,  paracheck=FALSE)
    tmf <- ecoy; tmf[tmf <= lo] <- lo; tmf[tmf >= hi] <- hi
    Ysec0o <- lmomco::par2qua(tmf,  ypara,  paracheck=FALSE)
  if(USE_PARA_COP) {
    tmf <- dtty; tmf[tmf <= lo] <- lo; tmf[tmf >= hi] <- hi
    Ysdtt0 <- lmomco::par2qua(tmf,  ypara,  paracheck=FALSE)
  }

  # The suppressWarnings() because of this
  #   # Warning message:  # when ties exist in the incoming to approx().
  # In regularize.values(x, y, ties, missing(ties), na.rm = na.rm):collapsing to unique 'x' values
  #suppressWarnings( Ysdd <- approx(lmomco::par2qua(dd, xpara), Ysdd0, xout=sortX, rule=2)$y )

  if(any(! is.na(xout))) {  # BEGIN BLOCK OF ORGANIC PREDICTIONS BY COPULA
    # Back us off from -Inf and +Inf to NaN results when a probability = 0 | 1, to ensure numerical
    # return on the approx() application.
    if(verbose) message("(", i <- i + 1, ") solving for Y predictions along given Xs and given (if any) Xouts")
      tmf <- ec;  tmf[tmf <= lo]  <- lo; tmf[tmf >= hi] <- hi
      suppressWarnings( Ysec      <- approx(lmomco::par2qua(tmf, xpara, paracheck=FALSE), Ysec0, xout=sortX,  rule=2)$y )
      suppressWarnings( Ysec_out  <- approx(lmomco::par2qua(tmf, xpara, paracheck=FALSE), Ysec0, xout=xout,   rule=2)$y )
      tmf <- eco; tmf[tmf <= lo]  <- lo; tmf[tmf >= hi] <- hi
      suppressWarnings( Yseco     <- approx(lmomco::par2qua(tmf, xpara, paracheck=FALSE), Ysec0o, xout=sortX, rule=2)$y )
      suppressWarnings( Ysec_outo <- approx(lmomco::par2qua(tmf, xpara, paracheck=FALSE), Ysec0o, xout=xout,  rule=2)$y )
    if(USE_PARA_COP) {
      tmf <- dtt; tmf[tmf <= lo]  <- lo; tmf[tmf >= hi] <- hi
      suppressWarnings( Ysdtt     <- approx(lmomco::par2qua(tmf, xpara, paracheck=FALSE), Ysdtt0, xout=sortX, rule=2)$y )
      suppressWarnings( Ysdtt_out <- approx(lmomco::par2qua(tmf, xpara, paracheck=FALSE), Ysdtt0, xout=xout,  rule=2)$y )
      names(Ysdtt)     <- NULL
      names(Ysdtt_out) <- NULL
    }

    names(Ysec)      <- NULL
    names(Yseco)     <- NULL
    names(Ysec_out)  <- NULL
    names(Ysec_outo) <- NULL
  } # END BLOCK OF ORGANIC PREDICTIONS BY COPULA

  if(plotxy) { # real-world unit plotting
        myxlim <- range(c(xp, lmomco::par2qua(FFedge, xpara, paracheck=FALSE)),      na.rm=TRUE)
        myylim <- range(c(yp, lmomco::par2qua(FFedge, ypara, paracheck=FALSE)),      na.rm=TRUE)
    if(limout) {
        myxlim <- range(c(myxlim, xout),                                             na.rm=TRUE)
        myylim <- range(c(myylim, Yloc, Yloc_out, Ysec, Yseco, Ysec_out, Ysec_outo), na.rm=TRUE)
      if(USE_PARA_COP) {
        myylim <- range(c(myylim, Ysdtt, Ysdtt_out), na.rm=TRUE)
      }
    }
    if(! is.null(xlim)) {
      if(length(xlim) == 2 & ! any(is.na(xlim))) {
        myxlim <- xlim
      } else {
        myxlim <- range(c(xlim, myxlim), na.rm=TRUE)
      }
    }
    if(! is.null(ylim)) {
      if(length(ylim) == 2 & ! any(is.na(ylim))) {
        myylim <- ylim
      } else {
        myylim <- range(c(ylim, myylim), na.rm=TRUE)
      }
    }
    if(! addxy) {
      if(verbose) message("(", i <- i + 1, ") plotting the analyses on the (X,Y) domain")
      plot(xp, yp, type="n", xlim=myxlim, ylim=myylim, las=1, mgp=c(2.2, 0.75, 0),
                   xlab="X OF DISTRIBUTION", ylab="Y OF DISTRIBUTION")
      mtext(titlexy, side=3, line=1, font=2, cex=titlecex)
      axis(3, axTicks(1), labels=FALSE, lwd=NA, lwd.ticks=1)
      axis(4, axTicks(2), labels=FALSE, lwd=NA, lwd.ticks=1)
    }
    if(rugxy) {
      suppressWarnings( rug(thex, ticksize=0.02, side=1, col=RUGcol, lwd=ruglwd) )
      suppressWarnings( rug(they, ticksize=0.02, side=2, col=RUGcol, lwd=ruglwd) )
      # Warning message: In rug(thex, ticksize = 0.02, side = 1, col = RUGcol, lwd = ruglwd) :
      #                  some values will be clipped
    }
      points(xp, yp, pch=DATpch, cex=DATcex, col=DATcol, bg=DATbg, lwd=DATptlwd)
      abline(locpara$loc_lmr[1], locpara$loc_lmr[2], lty=LOCUlty,  lwd=LOCUlwd, col=LOCUcol, lend=2)
      abline(locpair$loc_lmr[1], locpair$loc_lmr[2], lty=LOClty,   lwd=LOClwd,  col=LOCcol,  lend=2)
    if(USE_PARA_COP) {
      tmf <- dtt; tmf[tmf <= lo] <- lo; tmf[tmf >= hi] <- hi # assurance policy
      lines(lmomco::par2qua(tmf, xpara, paracheck=FALSE), Ysdtt0, lty=COPlty, lwd=COPlwd, col=COPcol, lend=2)
    }
      tmf <- ec; tmf[tmf <= lo] <- lo; tmf[tmf >= hi] <- hi # assurance policy
      lines(lmomco::par2qua(tmf,  xpara, paracheck=FALSE), Ysec0,     lwd=DIAlwd,  col=DIAcol, lend=2)
      points(sortX,    Yseco,   pch=PCHcty, cex=PCHcex, col=PCHcol, lwd=PCHlwd,   bg=PCHbg)
      points(sortX,    Ysec,    pch=DIApch, cex=DIAcex, col=DIAcol, lwd=DIAptlwd, bg=DIAbg)
    if(! addxy & any(! is.na(xout))) { # to remain consistent with no messaging if plot() not called
      if(verbose) message("(", i <- i + 1, ") drawing the organic predictions in (X,Y) domain")
      points(xout,   Yloc_out,  pch=LOCpch, cex=LOCcex, col=OUTcol, lwd=LOCptlwd, bg=LOCbg)
      points(xout,   Ylocu_out, pch=LOCpch, cex=LOCcex, col=OUTcol, lwd=LOCptlwd, bg=LOCbg)
      points(xout,   Ysec_out,  pch=OUTpch, cex=OUTcex, col=OUTcol, lwd=OUTptlwd, bg=LOCbg)
      if(USE_PARA_COP) {
        points(xout, Ysdtt_out, pch=COPpch, cex=COPcex, col=COPcol, lwd=COPptlwd, bg=COPbg)
      }
    }

    if(autoleg) {
      lwd    <- c(LOClwd, LOCUlwd, COPlwd, DIAlwd,       NA,     NA,       NA,      NA,       NA,        NA)
      lty    <- c(LOClty, LOCUlty, COPlty, DIAlty,       NA,     NA,       NA,      NA,       NA,        NA)
      col    <- c(LOCcol, LOCUcol, COPcol, DIAcol,   DIAcol, PCHcol,   DATcol,   OUTcol,   OUTcol,   COPcol)
      pch    <- c(NA,          NA,     NA,     NA,   DIApch, PCHcty,   DATpch,   LOCpch,   OUTpch,   COPpch)
      pt.bg  <- c(NA,          NA,     NA,     NA,    DIAbg,  PCHbg,    DATbg,    OUTbg,    OUTbg,    COPbg)
      pt.lwd <- c(NA,          NA,     NA,     NA, DIAptlwd, PCHlwd, DATptlwd, LOCptlwd, OUTptlwd, COPptlwd)
      pt.cex <- c(NA,          NA,     NA,     NA,   DIAcex, PCHcex,   DATcex,   LOCcex,   OUTcex,   COPcex)
      txt    <- c("Line of organic correlation (LOC) using paired data only and fit using linear moments",
                  "PARA-LOC fit by the linear moments of the marginal parametric distributions",
                  "Organic correlation by parametric asymmetric Plackett copula",
                  "Organic correlation by copula diagonal inverse (Kumaraswamy smooth)",
                  "Coordinates of organic correlation by empirical copula diagonal inverse",
                   ctyTXTxy,
                  paste0("Paired data points (n=", prettyNum(n, big.mark=","), ") between X and Y"),
                  "Either LOC predicted Y for requested X on LOC or Ultra-LOC as plotting shows",
                  "Organic correlation predicted Y for requested X through empirical copula",
                  "Organic correlation predicted Y for requested X through parametic copula")
      if(is.null(xout) | length(xout[! is.na(xout)]) == 0) {
        nix <- -c(8, 9, 10)
        txt    <- txt[   nix]
        lwd    <- lwd[   nix]
        lty    <- lty[   nix]
        col    <- col[   nix]
        pch    <- pch[   nix]
        pt.bg  <- pt.bg[ nix]
        pt.lwd <- pt.lwd[nix]
        pt.cex <- pt.cex[nix]
      }
      if(! USE_PARA_COP) {
        nix <- -c(3, length(txt))
        txt    <- txt[   nix]
        lwd    <- lwd[   nix]
        lty    <- lty[   nix]
        col    <- col[   nix]
        pch    <- pch[   nix]
        pt.bg  <- pt.bg[ nix]
        pt.lwd <- pt.lwd[nix]
        pt.cex <- pt.cex[nix]
      }
      if(! kumaraswamy) {
        nix <- -grep("Kumaraswamy", txt)
        txt    <- txt[   nix]
        lwd    <- lwd[   nix]
        lty    <- lty[   nix]
        col    <- col[   nix]
        pch    <- pch[   nix]
        pt.bg  <- pt.bg[ nix]
        pt.lwd <- pt.lwd[nix]
        pt.cex <- pt.cex[nix]
      }
      opts <- par(no.readonly=TRUE)
      par(lend=2)
      legend(xleg, yleg, txt, bty="o", cex=LEGcex, inset=LEGinset, seg.len=SEGlen, box.col=NA,
          bg="white", lwd=lwd, lty=lty, col=col, pch=pch, pt.bg=pt.bg, pt.lwd=pt.lwd, pt.cex=pt.cex)
      par(opts)
    }
  }

  df1 <- data.frame( xout=xout, locpair =Yloc_out, locpara     =Ylocu_out,
                                bicoploc=Ysec_out, bicoploc_emp=Ysec_outo )
  df2 <- data.frame( jtprob=ff, uv=ec, uv_emp=eco)
  if(USE_PARA_COP) {
    df1$bicoploc_cop <- Ysdtt_out
    df2$uv_cop       <-   dtt
  }
  df1 <- round(df1, digits=locdigits)
  df2 <- round(df1, digits=8)

        dtypes  <- c( dtypex,   dtypey )
  names(dtypes) <- c("dtypex", "dtypey")
  faqs <- c(n, n_thex, n_they)
  names(faqs) <- c("paired_sample_size", "nonmissing_x_sample_size", "nonmissing_y_sample_size")

  locsols <- list(locpair=locpair, locpara=locpara)

  zz <- list(organic=df1, locsols=locsols, xpara=xpara, ypara=ypara, dtypes=dtypes,
                          faqs=faqs, faqscop=faqscop, diag=df2)
  return(zz)
}

#set.seed(4); nsim <- 50
#X  <- rnorm(nsim, mean=3, sd=0.6)
#Y  <- rnorm(nsim, mean=0, sd=0.2)
#zz <- bicoploc(X,Y, xout=c(2.5, 3.5, 4), dtypex="nor", dtypey="gov")
#
#set.seed(4); nsim <- 50
#X  <- rexp(nsim, rate=3)
#Y  <- 0.7*X + rnorm(nsim, mean=0, sd=0.2)
#zz <- bicoploc(X,Y, xout=c(2.5, 3.5, 4), dtypex="exp", dtypey="gev")
