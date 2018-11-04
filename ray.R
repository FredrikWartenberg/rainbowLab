## ray
ray <-function(O=c(0,0,0),D=c(1,0,0),color="yellow",alpha="1")
{
    ray <- list('O'     =  O,
                 'D'     = D,
                 'color' = color,
                 'alpha' = alpha)
    class(ray) <- "ray"
    return(ray)
}

point.ray <- function(ray,t)
{
    ray$O + c(t) * ray$D
}

render.ray <- function(ray,t)
{
    p0=ray$O
    p1 <- p0 + ray$D * c(t)
    ##arrow3d(p0,p1,s=1/20,color=ray$color)
    arrow3d(point.ray(ray,0),
            point.ray(ray,t),
            s=1/20,
            color=ray$color)
}

intersect.ray <- function(ray,oShape)
{

    ## Mitternachtsformel..
    a=1
    b = 2 * ray$D %*% (ray$O - oShape$O)
    c = (ray$O - oShape$O) %*% (ray$O - oShape$O) - oShape$R^2

    ## Discriminat
    d <- b^2 - 4*a*c

    ## determine intersections
    res = list()
    if(d > 0)
    {
        res <- c(res, 't1' = (-b + sqrt(d))/2)
        res <- c(res, 't2' = (-b - sqrt(d))/2)
    } else if ( d == 0 )
    {
        res <- c(res, 't1' = -b/2)
    } else
    {
    }

    return(res)
}

rotationMatrix <-function(r,theta)
{
    ## rotation matrix
    m <- c(0,-r[3], r[2], r[3], 0, -r[1], -r[2], r[1],0)
    MM <- matrix(m ,3,3,byrow=TRUE)
    M <- r %*% t(r)
    I <-  diag(3)
    TM <- (1- cos(theta)) * M + cos(theta) * I + sin(theta) * MM
    return(round(TM,3))
}


refract <-function(ray,t,drop)
{
    ## calculate interaction point
    iP <- point.ray(ray,t)

    ## calculate normal on drop
    nD <- normal.drop(drop,iP)

    ## calculate normal to refraction plane
    nR <- cross(iP - drop$O,ray$D)
    nR <- nR * c(1/norm(nR,type="2"))

    ## calculate theta, incident angle
    thetaI = abs(c(acos(ray$D %*% nD)))
    if(thetaI > pi/2)
    {
        thetaI <- pi - thetaI
    }

    ## calculate excident angle Outside --> In
    thetaE <- asin(sin(thetaI/drop$n))

    ## calculate rotation of ray
    dTheta <- thetaE - thetaI

    ## create new rotated ray
    rA <- pi + thetaE
    rm <- rotationMatrix(nR,rA)
    ##    rRay <- ray(O=iP,nD %*% rm,color="grey")
    rRay <- ray(O=iP,(nD %*% rm) ,color="grey")

    return(rRay)

}

reflect <-function(ray,t,drop)
{
    browser()
    ## calculate interaction point
    iP <- point.ray(ray,t)

    ## calculate normal on drop
    nD <- normal.drop(drop,iP)

    ## calculate normal to refraction plane
    nR <- cross(iP - drop$O,ray$D)
    nR <- nR * c(1/norm(nR,type="2"))

    ## calculate theta, incident angle
    cv <- as.vector(ray$D) %*% as.vector(nD)
    ##    thetaI = abs(c(acos(ray$D %*% nD)))
    thetaI = abs(c(acos(cv)))
    if(thetaI > pi/2)
    {
        thetaI <- pi - thetaI
    }


    ## create new rotated ray
    rA <- - thetaI
    rm <- rotationMatrix(nR,rA)
    ##    rRay <- ray(O=iP,nD %*% rm,color="grey")
    rRay <- ray(O=iP,(nD %*% rm) ,color="yellow")

    return(rRay)

}


