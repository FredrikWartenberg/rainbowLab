launch <- function(ray,drop,maxInteractions=3,render=TRUE,debug=TRUE)
{

    ## scene graph
    scg <- sceneGraph()
    ni = 0

    ## first interaction is from out side to inside
    i1 <-intersect(ray,drop)
    ## check if type of interaction is relevant
    ts <- unlist(i1)
    if(length(ts) != 2)
    {
        ## break as no or tangential interaction
        return(ni)
    }
    ni = ni +1

    ## intersection point (first intersectionn)
    t <- min(ts)
    scg[['rayToIntersection']] <- getShape(ray,t)
    ip <- point.ray(ray,t)

    ## normal vector [debug]
    nv <- normal.drop(drop,ip)
    ##scg[['intersectionNormal']] <- arrow(ip,ip+nv*drop$R,s=1/4,color="red")

    ## refraction plane [debug]
    rpn <- cross(ip-drop$O,ip)
    rpn  <- rpn * c(1/norm(rpn,type="2"))
    ##scg[['refractionPlane']] <- plane(rpn,d = - rpn %*% ip, alpha=0.1)

    ## refract into drop
    o2i <- refract(ray,t,drop,dir="o2i")
    t <- max(unlist(intersect(o2i,drop)))
    scg[['rayIntoDrop']] <- getShape(o2i,t)
    ni = ni + 1

    ## do the reflections
    ir <- o2i
    for(i in 2:(maxInteractions-1))
    {
        i2i <- reflect(ir,t,drop)
        t <- max(unlist(intersect(ir,drop)))
        name = paste("reflection",i-1,sep="")
        scg[[name]] <- getShape(i2i,t)
        ir <- i2i
        ni = ni + 1
    }

    ## refract out of ray
    i2o <- refract(ir,t,drop,dir="i2o")
 ##   scg[['extingRay']] <- getShape(i2o,16000)
    ## plot normal vector
    ep <- point.ray(ir,t)
    nv2 <- normal.drop(drop,ep)

    return(list('ray' = i2o, 'scg'= scg))
}

## Catch rays on a surfce
catch <- function(ray,screen)
{

}

## Fan out rays from exit point
fanOut <- function(ray,t)
{
    getShape(ray,t)
}

follow <- function(ray,drop,t=10000,nInt=3,id=0)
{
    s1 <- launch(ray,rDrop,maxInteractions=nInt)
    name = paste('r',id,"fanOut",sep="")
    s1$scg[[name]] <- fanOut(s1$ray,t)
    s1$scg
}
