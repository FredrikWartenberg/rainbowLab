## Rotation around arbitrary axis r with angle theta
## source: http://ksuweb.kennesaw.edu/~plaval/math4490/rotgen.pdf
rotationMatrix <-function(r,theta)
{
    ## rotation matrix
    m <- c(0,-r[3], r[2], r[3], 0, -r[1], -r[2], r[1],0)
    MM <- matrix(m ,3,3,byrow=TRUE)
    M <- r %*% t(r)
    I <-  diag(3)
    TM <- (1- cos(theta)) * M + cos(theta) * I + sin(theta) * MM
    return(TM)
}

## x - v define plane, normal vector is z
## returns matrix with orthogonal unit vectors
coordSystem <- function(x,v)
{
    xHat <- x
    zHat <- pracma::cross(xHat,v)
    if(norm(zHat,type="2") == 0)
    {
        zHat <- c(-x[2],x[1],0)
        ## warning(paste("Coord Sys: x ", x, "and v", "are linearly dependent"))
    }

    zHat <- zHat / norm(zHat,type="2")
    yHat <- pracma::cross(zHat,xHat)
    yHat <- yHat / norm(yHat,type="2")
    return(t(rbind(xHat,yHat,zHat)))
}


## Signed angle
angle <- function(CM,v)
{
    v = v/norm(v,type="2")
    ang = acos(v %*% CM[,1])
    ##  ang = acos(v %*% xHat)
    ## determine sign (use determinant)
    ang = sign(det(rbind(CM[,1],v,CM[,3]))) * ang
    ##    ang = sign(det(rbind(xHat,v,zHat))) * ang
    return(ang)
}
