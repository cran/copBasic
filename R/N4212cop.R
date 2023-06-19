"N4212cop" <-
function(u, v, para=NULL, infis=100, ...) {
    TT <- para[1]

    if(is.null(para)) {
       warning("Empty para argument, need value on [1,Inf)")
       return()
    }

    if(TT < 1) {
       warning("Theta < 1, invalid parameter")
       return()
    }

    if(TT == 1)    return(PSP(u,v)) # the PSP copula
    if(TT > infis) return(  M(u,v)) # upper copula bounds

    cop <- ( u^-1 - 1 )^TT + (v^-1 - 1)^TT
    cop <- ( 1 + cop^(1/TT) )^-1

    return(cop)
}

