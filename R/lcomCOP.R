"lcomCOP" <-
function(cop=NULL, para=NULL, as.bilmoms=FALSE, orders=2:5, lo=0, hi=1,
         subdivisions=100L, rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol,
         stop.on.error=TRUE, ...) {
   if(is.null(cop)) {
      warning("must have copula argument specified, returning NULL")
      return(NULL)
   }
   if(any(orders < 2) | any(orders > 5)) {
     warning("orders < 2 are not logicaly or > 5 are not supported")
     return(NULL)
   }

   orders <- orders - 1
   r <- ifelse(as.bilmoms, 0, 1)
   func1 <- function(t) {          2                       }
   func2 <- function(t) { ( 12*t   -   6)                  }
   func3 <- function(t) { ( 60*t^2 -  60*t   +  12)        }
   func4 <- function(t) { (280*t^3 - 420*t^2 + 180*t - 20) }

   deltasX1wrtX2 <- deltasX2wrtX1 <- rep(NA,4+r)
   zz <- list()

   if(any(orders == 1)) { myint <- NULL
     try(myint <- integrate(function(u) {
                   sapply(u,function(u) { integrate(function(v) {
                   func1(v)*COP(u,v,cop=cop, para=para, ...) }, lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error)$value })},
                                                                lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error) )
     deltasX1wrtX2[1+r] <- ifelse(is.null(myint), NA, myint$value - 0.5)
   }
   if(any(orders == 2)) { myint <- NULL
     try(myint <- integrate(function(u) {
                   sapply(u,function(u) { integrate(function(v) {
                   func2(v)*COP(u,v,cop=cop, para=para, ...) }, lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error)$value })},
                                                                lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error) )
     deltasX1wrtX2[2+r]  <- ifelse(is.null(myint), NA, myint$value - 0.5)
   }
   if(any(orders == 3)) { myint <- NULL
     try(myint <- integrate(function(u) {
                   sapply(u,function(u) { integrate(function(v) {
                   func3(v)*COP(u,v,cop=cop, para=para, ...) }, lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error)$value })},
                                                                lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error) )
     deltasX1wrtX2[3+r]  <- ifelse(is.null(myint), NA, myint$value - 0.5)
   }
   if(any(orders == 4)) { myint <- NULL
     try(myint <- integrate(function(u) {
                   sapply(u,function(u) { integrate(function(v) {
                   func4(v)*COP(u,v,cop=cop, para=para, ...) }, lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error)$value })},
                                                                lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error) )
     deltasX1wrtX2[4+r] <- ifelse(is.null(myint), NA, myint$value - 0.5)
   }

   if(any(orders == 1)) { myint <- NULL
     try(myint <- integrate(function(u) {
                   sapply(u,function(u) { integrate(function(v) {
                   func1(u)*COP(u,v,cop=cop, para=para, ...) }, lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error)$value })},
                                                                lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error) )
     deltasX2wrtX1[1+r] <- ifelse(is.null(myint), NA, myint$value - 0.5)
   }
   if(any(orders == 2)) { myint <- NULL
     try(myint <- integrate(function(u) {
                   sapply(u,function(u) { integrate(function(v) {
                   func2(u)*COP(u,v,cop=cop, para=para, ...) }, lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error)$value })},
                                                                lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error) )
     deltasX2wrtX1[2+r] <- ifelse(is.null(myint), NA, myint$value - 0.5)
   }
   if(any(orders == 3)) { myint <- NULL
     try(myint <- integrate(function(u) {
                   sapply(u,function(u) { integrate(function(v) {
                   func3(u)*COP(u,v,cop=cop, para=para, ...) }, lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error)$value })},
                                                                lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error) )
     deltasX2wrtX1[3+r] <- ifelse(is.null(myint), NA, myint$value - 0.5)
   }
   if(any(orders == 4)) { myint <- NULL
     try(myint <- integrate(function(u) {
                   sapply(u,function(u) { integrate(function(v) {
                   func4(u)*COP(u,v,cop=cop, para=para, ...) }, lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error)$value })},
                                                                lo, hi,
                                        subdivisions=subdivisions, rel.tol=rel.tol, abs.tol=abs.tol,
                                        stop.on.error=stop.on.error) )
     deltasX2wrtX1[4+r] <- ifelse(is.null(myint), NA, myint$value - 0.5)
   }

   if(as.bilmoms) {
     names(deltasX1wrtX2) <- c("BiVarLM:del1[12]", "BiVarLM:del2[12]",
                               "BiVarLM:del3[12]", "BiVarLM:del4[12]")
     names(deltasX2wrtX1) <- c("BiVarLM:del1[21]", "BiVarLM:del2[21]",
                               "BiVarLM:del3[21]", "BiVarLM:del4[21]")
     zz$bilmomUV <- deltasX1wrtX2; zz$bilmomVU <- deltasX2wrtX1
   } else {
     names(deltasX1wrtX2) <- c("nothing",
                               "Lcomom:T2[12]", "Lcomom:T3[12]",
                               "Lcomom:T4[12]", "Lcomom:T5[12]")
     names(deltasX2wrtX1) <- c("nothing",
                               "Lcomom:T2[21]", "Lcomom:T3[21]",
                               "Lcomom:T4[21]", "Lcomom:T5[21]")
     zz$lcomUV <- 6*deltasX1wrtX2; zz$lcomVU <- 6*deltasX2wrtX1
   }
   return(zz)
}

# para <- list(cop=c(M, M, M, M), para=NULL, part=c(0,0.25,0.50,0.75,1))
# lcomCOP(cop=ORDSUMcop,para=c(0,0.25,0.50,0.75,1) )
