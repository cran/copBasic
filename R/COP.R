"COP" <-
function(u, v, cop=NULL, para=NULL,
               reflect=c("cop", "surv", "acute", "grave",
                           "1",    "2",     "3",     "4"), ...) {
   reflect <- match.arg(reflect)
   if(is.list(para)) {
      reflect   <- para$reflect
      if(is.null(reflect)) reflect <- "cop"
      cop  <- para$cop
      para <- para$para
   }
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
