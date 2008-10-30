"duCOP" <-
function(cop=NULL, u, v, ...) {
    #str(cop)
    return(u + v - cop(u,v,...))
}
