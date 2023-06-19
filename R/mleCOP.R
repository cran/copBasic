"mleCOP" <-
function(u, v=NULL, cop=NULL, parafn=function(k) return(k),
          interval=NULL, init.para=NULL, verbose=FALSE, control=list(),
          the.zero=.Machine$double.eps^0.25, s=0, ...) {
   verbose <- as.integer(verbose)
   if(is.null(v)) {
      if(length(names(u)) != 2) {
         warning("a data.frame having only two columns is required")
         return(NULL)
      }
      v <- u[, 2]; u <- u[, 1] # v must come before u
   }
   if(length(u) != length(v)) {
      warning("argument(s) or implied arguments u and v are unequal in ",
              "length, returning NULL")
      return(NULL)
    }
    objfunc <- function(thetas, ...) {
       para <- parafn(thetas)
       if(verbose >= 2) {
         message("mleCOP---parameters (transformed to as copulas receive):")
         if(is.list(para)) {
           tmpp <- para
           tmpp[grep("cop", tmpp)] <- NULL
           for(p in seq_len(length(tmpp))) tmpp[[p]] <- round(tmpp[[p]], digits=8)
         } else {
           tmpp <- para
         }
         paratxt <- paste(names(tmpp), tmpp, sep="=", collapse=", ")
         message("         ", paratxt)
         if(length(s) >= 2) {
           t <- simCOPv(s, cop=cop, para=para)
           plot(s,t, xlab="U, NONEXCEEDANCE PROBABILITY",
                     ylab="V, NONEXCEEDANCE PROBABILITY", lwd=0.62)
           mtext(paratxt, font=2, line=0.5)
         }
       }
       copdf <- densityCOP(u,v, cop=cop, para=para, sumlogs=TRUE,
                                the.zero=the.zero, ...)
       if(verbose >= 1) message("  densityCOP---sumlogs = ", round(copdf, digits=8))
       return(copdf)
    }
    SMALL <- 0.01 # a one percent closeness
    rt <- NULL
    if(! is.null(interval)) {
       # need optimise import!!! stats::optimise
       try(rt <- optimise(f=objfunc, interval=interval, maximum=TRUE, ...))
       if(! is.null(rt)) {
         the.rt <- rt$maximum
         if(abs(1 - the.rt/min(interval)) < SMALL) {
            warning("lower bounds might be too close to solution")
         }
         if(abs(1 - the.rt/max(interval)) < SMALL) {
            warning("upper bounds might be too close to solution")
         }
         rt$packagetext <- "Solution by package copBasic + stats::optimise()"
         rt$para <- parafn(the.rt)
         rt$loglik <- rt$objective
         rt$AIC  <- 2*length(rt$para)                - 2*rt$loglik
         rt$BIC  <-   length(rt$para)*log(length(u)) - 2*rt$loglik
         return(rt)
       } else {
         return(NULL)
       }
    } else if(! is.null(init.para)) {
       control$fnscale <- -1
       if(length(init.para) == 1) {
          warning("init.para is length one, need to use 1D 'Brent' method\n",
                  " but then the interval **will require** specification,\n",
                  " proceeding anyway with init.para and optim() call,\n",
                  " other warning message will be triggered by optim(),\n",
                  " suggest calling > mleCOP(..., interval=c(N,M), ...)")
       }
       try(rt <- optim(init.para, fn=objfunc, control=control, ...))
       if(! is.null(rt)) {
         rt$packagetext <- "Solution by package copBasic + stats::optim()"
         rt$para <- parafn(rt$par)
         rt$loglik <- rt$value
         rt$AIC  <- 2*length(rt$para)                - 2*rt$loglik
         rt$BIC  <-   length(rt$para)*log(length(u)) - 2*rt$loglik
         return(rt)
       } else {
         return(NULL)
       }
    } else {
       warning("ambiguous search criteria for either interval or init.para")
       return(NULL)
    }
}
