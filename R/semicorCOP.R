"semicorCOP" <-
function(cop=NULL, para=NULL, truncation=0, n=0, as.sample=FALSE, ...) {
   rhoNs <- list(cor.normal.scores=NA,
                 botleft.semicor=NA,   topright.semicor=NA,
                 topleft.semicor=NA,   botright.semicor=NA,
                 type="TO BE FILLED IN BY CODE", source="semicorCOP")
   if(truncation < 0) {
      warning("inconsistent truncation argument, returning NULL")
   }
   method <- "pearson"
   a <- truncation
   if(as.sample) {
      if(is.null(para)) {
         warning("Sample semi-correlations are desired by 'para' but it is NULL, ",
                 "returning NULL")
         return(NULL)
      }
      if(length(names(para)) != 2) {
         warning("para argument must be data.frame having only two columns, ",
                 "returning NULL")
         return(NULL)
      }
      para <- para[para[,1] > 0 & para[,1] < 1 & para[,2] > 0 & para[,2] < 1, ]
      n <- length(para[,1])  # Hazen plotting positions
      para[,1] <- (rank(para[,1]) - 0.5) / n
      para[,2] <- (rank(para[,2]) - 0.5) / n
            qu <- qnorm(para[,1])
            qv <- qnorm(para[,2])
      rhoNs$cor.normal.scores <- cor(qu,qv, method=method)
      rhoNs$topright.semicor      <- cor(qu[qu >  a & qv >  a],
                                     qv[qu >  a & qv >  a], method=method)
      rhoNs$botleft.semicor     <- cor(qu[qu < -a & qv < -a],
                                     qv[qu < -a & qv < -a], method=method)
      rhoNs$botright.semicor <- cor(qu[qu >  a & qv < -a],
                                     qv[qu >  a & qv < -a], method=method)
      rhoNs$topleft.semicor <- cor(qu[qu < -a & qv >  a],
                                     qv[qu < -a & qv >  a], method=method)
      rhoNs$type <- "cor()'s on columns in 'para'"
   } else {
      if(is.null(cop)) {
         warning("must have copula argument specified, returning NULL")
         return(NULL)
      }
      if(n == 0) {
         warning("must have sample size argument specified, a section of code ",
                 "using integration is not implemented, returning NULL")
         return(NULL)
      } else {
         UV <- simCOP(n=n, cop=cop, para=para, graphics=FALSE, ...)
         para <- UV[UV[,1] > 0 & UV[,1] < 1 & UV[,2] > 0 & UV[,2] < 1, ]
         n <- length(para[,1]) # Hazen plotting positions
         para[,1] <- (rank(para[,1]) - 0.5) / n
         para[,2] <- (rank(para[,2]) - 0.5) / n
               qu <- qnorm(para[,1]);
               qv <- qnorm(para[,2])
         rhoNs$cor.normal.scores <- cor(qu,qv, method=method)
         rhoNs$topright.semicor      <- cor(qu[qu >  a & qv >  a],
                                        qv[qu >  a & qv >  a], method=method)
         rhoNs$botleft.semicor     <- cor(qu[qu < -a & qv < -a],
                                        qv[qu < -a & qv < -a], method=method)
         rhoNs$botright.semicor <- cor(qu[qu >  a & qv < -a],
                                        qv[qu >  a & qv < -a], method=method)
         rhoNs$topleft.semicor <- cor(qu[qu < -a & qv >  a],
                                        qv[qu < -a & qv >  a], method=method)
         rhoNs$type <- "simulated the copula, then computed cor()'s"
      }
   }
   return(rhoNs)
}
