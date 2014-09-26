"EMPIRcop" <-
function(u,v, para=NULL, bernstein=FALSE, bernsteinprogress=TRUE, ...) {
  if(exists("para", para)) {
     uobs <- para$para[,1];
     vobs <- para$para[,2];
     if(exists("bernstein", para))         bernstein         <- para$bernstein
     if(exists("bernsteinprogress", para)) bernsteinprogress <- para$bernsteinprogress
     para <- para$para # now reset the para to ONLY have the uobs and vobs because if
     # bernstein is triggered the a secondary call to this function is made and bernstein
     # must be turned off.
  }
  if(length(names(para)) != 2) {
    warning("a data.frame having only two columns is required in the para argument or para$para")
    return(NULL)
  }

  uobs <- para[,1];
  vobs <- para[,2];
  nobs <- length(uobs);

  nu <- length(u); nv <- length(v)
  if(nu > 1 & nv > 1 & nu != nv) {
    warning("length u = ",nu, " and length v = ",nv)
    warning("longer object length is not a multiple of shorter object length, no recycling in EMPIRcop()")
    return(NA)
  }
  # The extra hassle of vectorization made here is to handle situations
  # in which nested integrals are used where uneven vectors can be passed.
  if(nu == 1) {
     u <- rep(u, nv)
  } else if(nv == 1) {
     v <- rep(v, nu)
  }
  nu <- length(u) # make sure to RESET IT!

  if(bernstein) { # will potentially burn SERIOUS CPU time
     if(bernsteinprogress) {
         message("Bernstein triggered, index of each {u,v} shown: ", appendLF=FALSE);
     }
     ber <- sapply(1:nu, function(k) {
                   if(bernsteinprogress) message(k,"-", appendLF=FALSE);
                   tmpA <- sapply(1:nobs, function(i) {
                        A <- choose(nobs, i) * u[k]^i *(1-u[k])^(nobs-i)
                        tmpB <- sapply(1:nobs, function(j) {
                                  B <- choose(nobs, j) * v[k]^j *(1-v[k])^(nobs-j)
                                  return(EMPIRcop(i/nobs, j/nobs, para=para, bernstein=FALSE)*A*B) })
                                  return(sum(tmpB)) })
                        return(sum(tmpA)) })
     if(bernsteinprogress) message("\n", appendLF=FALSE);
     return(ber);
  } else {
     return(sapply(1:nu, function(k) {
           empcop <- sapply(1:nobs, function(i) {
                        return(as.numeric(uobs[i] <= u[k] & vobs[i] <= v[k])) });
           return(sum(empcop)/nobs); }))
  }
}

