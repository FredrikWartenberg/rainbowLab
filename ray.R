## ray
ray <-function(x=0,y=100,z=0,D=c(1,0,0),t=1,color="yellow",alpha="1")
{
    ray <- list('O'     =  c(x,y,z),
                 'D'     = D,
                 'color' = color,
                 'alpha' = alpha,
                 't'     =  t)
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
    TM <- (1- cos(theta)) * MM + cos(theta) * I + sin(theta) * M
    return(round(TM,3))
}



## old
intersect.old <- function(ray,oShape)
{

    a=1
    b = 2 * ray$D %*% (ray$O - oShape$O)
    c = (ray$O - oShape$O) %*% (ray$O - oShape$O) - oShape$R^2

    d <- b^2 - 4*a*c

    nv <- function(p,o=oShape$O)
    {
        nv <- (p-o)
        sf <- 1/norm(nv,type = "2")
        nv <-  nv * c(sf)
    }

    r1 = NULL
    if(d > 0)
    {
        t1 <- (-b + sqrt(d))/2
        p1 <- point.ray(ray,t1)
        n1 <- nv(p1)
        r1 <- p1-oShape$O
        t2 <- (-b - sqrt(d))/2
        p2 <- point.ray(ray,t2)
        n2 <- nv(p2)
    } else if ( d == 0 )
    {
        t1 <- -b/2
        p1 <- point.ray(ray,t1)
        n1 <- nv(p1)
        t2 <- NULL
        p2 <- NULL
    } else
    {
        t1 <- NULL
        p1 <- NULL
        t2 <- NULL
        p2 <- NULL
    }

    return(list('t1' = t1, 'p1' = p1, 'n1' = n1,'r1'=r1,
                't2' = t2, 'p2' = p2, 'n2' = n2))
}
