blomatrixCOPdec <-
function(cop=NULL, para=NULL, as.sample=FALSE, as.blomCOPss=TRUE,
                  ctype=c("weibull", "hazen", "1/n",
                          "bernstein", "checkerboard"), ...) {
  t <- c(0.10, 0.50, 0.90)
  if(as.sample) {
    ctype <- match.arg(ctype)
    if(is.null(para)) {
      warning("Sample Blomqvist's Beta desired but para is NULL, returning NULL")
      return(NULL)
    }
    if(length(names(para)) != 2) {
      warning("para argument must be data.frame having only two columns, returning NULL")
      return(NULL)
    }
    if(as.blomCOPss) {
      blom <- matrix(c(
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[1], t[1]), vv=c(t[1], t[1]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[2], t[1]), vv=c(t[2], t[1]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[3], t[1]), vv=c(t[3], t[1]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[1], t[2]), vv=c(t[1], t[2]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[2], t[2]), vv=c(t[2], t[2]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[3], t[2]), vv=c(t[3], t[2]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[1], t[1]), vv=c(t[1], t[3]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[2], t[1]), vv=c(t[2], t[3]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[3], t[1]), vv=c(t[3], t[3]), ...)
      ), ncol=3)
      colnames(blom) <- paste0("U|V=", c("0.10", "0.50", "0.90"))
      rownames(blom) <- rev(colnames(blom))
      return(blom)
    } else {
      A <- 1 / P(t, t[3]); B <- 1 / P(t, t[2]); C <- 1 / P(t, t[1])
      blom <- matrix(c(EMPIRcop(t, t[3], para=para, ctype=ctype, ...) * A - 1,
                       EMPIRcop(t, t[2], para=para, ctype=ctype, ...) * B - 1,
                       EMPIRcop(t, t[1], para=para, ctype=ctype, ...) * C - 1), ncol=3)
      colnames(blom) <- paste0("U|V=", c("0.10", "0.50", "0.90"))
      rownames(blom) <- rev(colnames(blom))
      return(blom)
    }
  } else {
    if(is.null(cop)) {
      warning("must have copula argument specified, returning NULL")
      return(NULL)
    }
    if(as.blomCOPss) {
      blom <- matrix(c(
          blomCOPss(cop=cop, para=para, uu=c(t[1], t[1]), vv=c(t[1], t[1]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[2], t[1]), vv=c(t[2], t[1]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[3], t[1]), vv=c(t[3], t[1]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[1], t[2]), vv=c(t[1], t[2]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[2], t[2]), vv=c(t[2], t[2]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[3], t[2]), vv=c(t[3], t[2]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[1], t[1]), vv=c(t[1], t[3]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[2], t[1]), vv=c(t[2], t[3]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[3], t[1]), vv=c(t[3], t[3]), ...)
      ), ncol=3)
      colnames(blom) <- paste0("U|V=", c("0.10", "0.50", "0.90"))
      rownames(blom) <- rev(colnames(blom))
      return(blom)
    } else {
      A <- 1 / P(t, t[3]); B <- 1 / P(t, t[2]); C <- 1 / P(t, t[1])
      blom <- matrix(c(cop(t, t[3], para=para, ...) * A - 1,
                       cop(t, t[2], para=para, ...) * B - 1,
                       cop(t, t[1], para=para, ...) * C - 1), ncol=3)
      colnames(blom) <- paste0("U|V=", c("0.10", "0.50", "0.90"))
      rownames(blom) <- rev(colnames(blom))
      return(blom)
    }
  }
}


blomatrixCOPiqr <-
function(cop=NULL, para=NULL, as.sample=FALSE, as.blomCOPss=TRUE,
                  ctype=c("weibull", "hazen", "1/n",
                          "bernstein", "checkerboard"), ...) {
  t <- c(0.25, 0.50, 0.75)
  if(as.sample) {
    ctype <- match.arg(ctype)
    if(is.null(para)) {
      warning("Sample Blomqvist's Beta desired but para is NULL, returning NULL")
      return(NULL)
    }
    if(length(names(para)) != 2) {
      warning("para argument must be data.frame having only two columns, returning NULL")
      return(NULL)
    }
    if(as.blomCOPss) {
           blom <- matrix(c(
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[1], t[1]), vv=c(t[1], t[1]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[2], t[1]), vv=c(t[2], t[1]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[3], t[1]), vv=c(t[3], t[1]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[1], t[2]), vv=c(t[1], t[2]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[2], t[2]), vv=c(t[2], t[2]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[3], t[2]), vv=c(t[3], t[2]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[1], t[1]), vv=c(t[1], t[3]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[2], t[1]), vv=c(t[2], t[3]), ...),
          blomCOPss(cop=EMPIRcop, para=para, uu=c(t[3], t[1]), vv=c(t[3], t[3]), ...)
      ), ncol=3)
      colnames(blom) <- paste0("U|V=", c("0.25", "0.50", "0.75"))
      rownames(blom) <- rev(colnames(blom))
      return(blom)
    } else {
      #A <- c(16/3, 16/6, 16/9)
      #B <- c(16/2, 16/4, 16/6)
      #C <- c(16/1, 16/2, 16/3)
      A <- 1 / P(t, t[3]); B <- 1 / P(t, t[2]); C <- 1 / P(t, t[1])
      blom <- matrix(c(EMPIRcop(t, t[3], para=para, ctype=ctype, ...) * A - 1,
                       EMPIRcop(t, t[2], para=para, ctype=ctype, ...) * B - 1,
                       EMPIRcop(t, t[1], para=para, ctype=ctype, ...) * C - 1), ncol=3)
      colnames(blom) <- paste0("U|V=", c("0.25", "0.50", "0.75"))
      rownames(blom) <- rev(colnames(blom))
      return(blom)
    }
  } else {
    if(is.null(cop)) {
      warning("must have copula argument specified, returning NULL")
      return(NULL)
    }
    if(as.blomCOPss) {
      blom <- matrix(c(
          blomCOPss(cop=cop, para=para, uu=c(t[1], t[1]), vv=c(t[1], t[1]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[2], t[1]), vv=c(t[2], t[1]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[3], t[1]), vv=c(t[3], t[1]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[1], t[2]), vv=c(t[1], t[2]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[2], t[2]), vv=c(t[2], t[2]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[3], t[2]), vv=c(t[3], t[2]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[1], t[1]), vv=c(t[1], t[3]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[2], t[1]), vv=c(t[2], t[3]), ...),
          blomCOPss(cop=cop, para=para, uu=c(t[3], t[1]), vv=c(t[3], t[3]), ...)
      ), ncol=3)
      colnames(blom) <- paste0("U|V=", c("0.25", "0.50", "0.75"))
      rownames(blom) <- rev(colnames(blom))
      return(blom)
    } else {
      #A <- c(16/3, 16/6, 16/9)
      #B <- c(16/2, 16/4, 16/6)
      #C <- c(16/1, 16/2, 16/3)
      A <- 1 / P(t, t[3]); B <- 1 / P(t, t[2]); C <- 1 / P(t, t[1])
      blom <- matrix(c(cop(t, t[3], para=para, ...) * A - 1,
                       cop(t, t[2], para=para, ...) * B - 1,
                       cop(t, t[1], para=para, ...) * C - 1), ncol=3)
      colnames(blom) <- paste0("U|V=", c("0.25", "0.50", "0.75"))
      rownames(blom) <- rev(colnames(blom))
      return(blom)
    }
  }
}
