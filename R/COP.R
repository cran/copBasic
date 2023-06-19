"COP" <-
function(u, v, cop=NULL, para=NULL,
               reflect=c("cop", "surv", "acute", "grave",
                           "1",    "2",     "3",     "4"), ...) {

   if(is.list(para) && ! is.null(para$cop) && is.null(cop)) {
      # The third test is related to whether cop is incoming from
      # the top level of the function. This is a major thing to
      # avoid overwriting the desired copula because
      # is.null(para$cop) is fuzzy meaning is.null(para$c) passes too.
      if(! is.null(para$reflect)) reflect <- para$reflect
      if(! is.null(para$cop    )) cop     <- para$cop
      if(! is.null(para$para   )) para    <- para$para
   }

   reflect <- as.character(reflect) # so that numeric integers would
   reflect <-    match.arg(reflect) #           silently be accepted

   if(! is.list(para)) names(para) <- NULL # removes unsightly named labels often on the para in this package
   return(switch(reflect,
                 cop   =             cop(u,     v, para=para, ...),
                 surv  = u + v - 1 + cop(1-u, 1-v, para=para, ...),
                 acute =     v     - cop(1-u,   v, para=para, ...),
                 grave = u         - cop(  u, 1-v, para=para, ...),
                 "1"   =             cop(u,     v, para=para, ...),
                 "2"   = u + v - 1 + cop(1-u, 1-v, para=para, ...),
                 "3"   =     v     - cop(1-u,   v, para=para, ...),
                 "4"   = u         - cop(  u, 1-v, para=para, ...)
         ))
}
