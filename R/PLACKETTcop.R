"PLcop" <-
function(u, v, para=NULL, ...) {
    TT <- para[1]
    if(is.null(para)) {
       warning("Empty para argument, need value on [0,Inf]")
       return()
    }
    if(TT < 0) {
       warning("Theta < 0, invalid parameter")
       return()
    }
    if(TT == 1)         return(  u*v)  # the product copula
    if(TT == 0)         return(W(u,v)) # lower copula bounds
    if(! is.finite(TT)) return(M(u,v)) # upper copula bounds
    cop <- 1 + (TT-1)*(u+v)
    suppressWarnings( tmp <- sqrt(cop^2 - 4*u*v*TT*(TT-1)) ) # December 2023
    # In sqrt(cop^2 - 4 * u * v * TT * (TT - 1)) : NaNs produced
    tmp[is.nan(tmp)] <- 0   # December 2023
    cop <- cop - tmp        # December 2023
    cop <- cop / (2*(TT-1))
    cop[cop < 0] <- 0       # December 2023
    cop[cop > 1] <- 1       # December 2023
    return(cop)
}

"PLACKETTcop" <- function(u, v, para=NULL, ...) {
   PLcop(u, v, para=para, ...)
}
