"EMPIRcop" <-
function(u,v, para=NULL, ...) {
  if(length(names(para)) != 2) {
    warning("a data.frame having only two columns is required")
    return(NULL)
  }
  uobs <- para[,1];
  vobs <- para[,2];
  n <- length(uobs);
  
  m <- length(u);
  if(m != length(v)) {
    warning("lengths of u and v are not identical")
    return(NULL) 
  }
  cop <- vector(mode="numeric", length=m);
  for(k in 1:m) {
     empcop <- sapply(1:n, function(i) {
                  return(as.numeric(uobs[i] <= u[k] & vobs[i] <= v[k])) });
     cop[k] <- sum(empcop)/n;
  }
  return(cop);
}

