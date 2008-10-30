"blomCOP" <-
function(cop=NULL, para=NULL, ...) {
   blom <- 4*cop(0.5,0.5, para=para, ...) - 1
   return(blom)
}

