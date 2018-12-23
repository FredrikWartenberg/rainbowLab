##' Generate one ray
##'
##' A ray represents a light ray. Its properties are
##' origin (O), direction (D), lambda and alpha.
##' @title ray
##' @param O Origin (cartesian coordinates)
##' @param D Direction (vector, will be normalised to 1
##' @param lambda wavelength [nm]
##' @param alpha alpha (for plotting)
##' @return ray object
##' @author Fredrik Wartenberg
ray <-function(O=c(0,0,0),D=c(1,0,0),lambda=600,alpha="1")
{
    ray <- list('O'      =  O,
                'D'      = D/norm(D,type="2"),
                'lambda' = lambda,
                'color'  = lambda2rgb(lambda),
                'alpha'  = alpha,
                's'      = 0.02
                )
    class(ray) <- "ray"
    return(ray)
}

reverse.ray <- function(ray)
{
 ##   ray$O
}
point.ray <- function(ray,t)
{
    as.vector(ray$O + c(t) * ray$D)
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


## rafarct into or out of drop
refract <-function(ray,t,drop,dir)
{
    ## calculate interaction point
    iP <- point.ray(ray,t)

    ## calculate normal on drop
    nD <- normal.drop(drop,iP)

    ## calculate normal to refraction plane
    nR <- pracma::cross(iP - drop$O,ray$D)
    nR <- nR * c(1/norm(nR,type="2"))

    ## calculate theta, incident angle
    thetaI = abs(c(acos(ray$D %*% nD)))
    if(thetaI > pi/2)
    {
        thetaI <- pi - thetaI
    }

    ## calculate excident angle Outside --> In
    n <- drop$ri(ray$lambda)
    if(dir == "o2i")
    {
        thetaE <- asin(sin(thetaI)/n)
        ## calc rotation angle
        rA <- pi + thetaE
        ##cat(paste("refraction o2i thetaI = " ,
        ##              round(thetaI*180/pi),
        ##              "thetaE =",
        ##              round(thetaE*180/pi), "\n"))

    } else if(dir == "i2o")
    {
        n <- drop$ri(ray$lambda)
        sinThetaE <- sin(thetaI)*n
        if(sinThetaE <= 1)
        {
            thetaE <- asin(sinThetaE)
            ## create new rotated ray
            rA <- - thetaE
            ## cat(paste("refraction i2o thetaI = " ,
            ##          round(thetaI*180/pi),
            ##          "thetaE =",
            ##          round(thetaE*180/pi), "\n"))
        } else
        {
            RA <- pi + thetaI
            ## cat("Total reflection i2o")
        }
    } else
    {
        stop(paste("Unkown direction", dir))
    }

    rm <- rotationMatrix(nR,rA)
    rRay <- ray(O=iP,as.vector((nD %*% rm)) ,lambda=ray$lambda)

    return(rRay)

}

## reflect inisde the drop
reflect <-function(ray,t,drop)
{

    ## calculate interaction point
    iP <- point.ray(ray,t)

    ## calculate normal on drop
    nD <- normal.drop(drop,iP)

    ## calculate normal to refraction plane
    nR <- pracma::cross(iP - drop$O,ray$D)
    nR <- nR * c(1/norm(nR,type="2"))

    ## calculate theta, incident angle
    cv <- as.vector(ray$D) %*% as.vector(nD)
    thetaI = abs(c(acos(cv)))
    if(thetaI > pi/2)
    {
        thetaI <- pi - thetaI
    }

    ## Brewster angle
    if(abs(thetaI) < atan(drop$ri(ray$lambda)))
    {
        ##cat(paste("No Total reflection", round(thetaI)))
        ##return(NULL)
    }


    ## create new rotated ray
    rA <- thetaI + pi
    rm <- rotationMatrix(nR,rA)
    rRay <- ray(O=iP,as.vector(nD %*% rm) ,lambda=ray$lambda)

    return(list('ray' = rRay, 'thetaI' = thetaI))

}


