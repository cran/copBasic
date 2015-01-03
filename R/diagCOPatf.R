"diagCOPatf" <-
 function(f, cop=NULL, para=NULL, verbose=FALSE, ...) {
    if(length(f) > 1) {
       warning("f argument is not a single value, only first value will be used")
       f <- f[1]
    }
    if(is.null(cop)) {
       warning("cop argument is NULL, returning NULL")
       return(NULL)
    }
    "afunc" <- function(uv) {
       thef <- cop(uv, uv, para=para, ...)
       return(f - thef)
    }
    rt <- NULL
    try(rt <- uniroot(afunc, interval=c(.Machine$double.eps, 1-.Machine$double.eps)))
    if(verbose) return(rt)
    return(rt$root)
 }

