"coCOP" <-
function(cop=NULL, u, v, exceedance=TRUE, ...) {
    #str(cop)
    if(! exceedance) {
       u <- 1 - u; v <- 1 - v
    }
    return(1 - cop(1-u,1-v,...))
}

