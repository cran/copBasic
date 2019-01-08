"nuskewCOP" <-
function(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...) {
   joeskewCOP(cop=cop, para=para, type="nu",
              brute=brute, delta=delta, as.sample=as.sample, ...)
}
"nustarCOP" <-
function(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...) {
   joeskewCOP(cop=cop, para=para, type="nustar",
              brute=brute, delta=delta, as.sample=as.sample, ...)
}
"joeskewCOP" <-
function(cop=NULL, para=NULL, type=c("nu", "nustar", "nuskew"), as.sample=FALSE,
                              brute=FALSE, delta=0.002,...) {
   type = match.arg(type)
   if(as.sample) {
      if(is.null(para)) {
         warning("Sample Nu-Skew desired but para is NULL, ",
                 "returning NULL")
         return(NULL)
      }
      if(length(names(para)) != 2) {
        warning("para argument must be data.frame having only two columns, ",
                "returning NULL")
        return(NULL)
      }

      n <- length(para[,1]); nn <- n^2; ns <- 1:n
      R <- rank(para[,1]); S <- rank(para[,2])
      corsgn <- sign(cor(para[,1],para[,2], method="kendall"))
      #npm <- ifelse(corsgn == -1, +1,   +0)
      spm <- ifelse(corsgn == -1, +2.4, +1.1)
      samNU <- NA
      if(type == "nu" | type == "nuskew") {
         if(as.sample == -1) message("Sample Nu-Skew after Joe (2014)",
                                     "---CPU intensive!")
         samNU <- sum(sapply(ns, function(i) {
                     sum(sapply(ns, function(j) {
                        (j - i)*sum(as.numeric(R <= i & S <= j))/(n+1)
                     } ))
                  } ))/(nn*(n)) # Note division by (n) but not in the similar place below
         return(96*samNU)
      } else if(type == "nustar") {
         if(as.sample == -1) message("Sample Nu-Skew-Star",
                                     "---CPU intensive!")
         samNU <- sum(sapply(ns, function(i) {
                  sum(sapply(ns, function(j) {
                     (j + i)*sum(as.numeric(R <= i & S <= j))/(n+spm)
                  } ))
               } ))/(nn*(n+1)) # Note division by (n+1) but not in the similar place above
         return(12*samNU - 4)
      } else {
         stop("Never should be here in logic")
      }
   }
   #NuStar: double sum of j+i is n^2*(n+1), hence the division shown above and note the
   # division by n within the R <= i & S <= j
   #h <- sapply(1:10, function(k)  { sapply(1:k, function(i) sapply(1:k, function(j) { (j+i) })) } )
   #sum(h[[10]])
   # Now compare to the j-i for which the double sum is 0, hence there is just the nn
   # division on the outside of the sum and there is not division by n for the R <= i & S <= j

   if(brute) {
      us <- vs <- seq(.Machine$double.eps, 1-.Machine$double.eps, delta)
      skew <- NA
      if(type == "nu" | type == "nuskew") {
         skew <- sum(sapply(us, function(u) {
                    sum(sapply(vs, function(v) {
                       (v-u)*COP(u,v,cop=cop,para=para, ...)
                    }))
                 }))
          return(96*skew*delta^2)
      } else if(type == "nustar") {
         skew <- sum(sapply(us, function(u) {
                    sum(sapply(vs, function(v) {
                       (v+u)*COP(u,v,cop=cop,para=para, ...)
                    }))
                 }))
          return(12*skew*delta^2 - 4)
      } else {
         stop("Never should be here in logic")
      }
      return(skew)
   }

   myint <- NULL
   if(type == "nu" | type == "nuskew") {
      try(myint <- integrate(function(u) {
               sapply(u,function(u) { integrate(function(v) {
                          (v-u)*COP(u,v,cop=cop, para=para,...)
               }, 0, 1)$value })}, 0, 1) )
      ifelse(is.null(myint), return(NA), return(96*myint$value))
   } else if(type == "nustar") {
      try(myint <- integrate(function(u) {
               sapply(u,function(u) { integrate(function(v) {
                          (v+u)*COP(u,v,cop=cop, para=para,...)
               }, 0, 1)$value })}, 0, 1) )
      ifelse(is.null(myint), return(NA), return(12*myint$value - 4))
   } else {
      stop("Never should be here in logic")
   }
}
