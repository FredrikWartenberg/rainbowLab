


## lauch a ray and follow it through the drop
launch <- function(ray,drop,parameters,maxInteractions=3)
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

    ## normal vector to drop
    nvD <- normal.drop(drop,ip)
    if(parameters$showNormals)
        scg[['intersectionNormal']] <- arrow(ip,ip+nvD*drop$R,s=1/8,color="black")

    ## coordinate system
    CSYS <- coordSystem(-nvD,ray$D)
    ## entry angle
    angIn <- angle(CSYS,ray$D)

    ## refraction plane [debug]
    rpn <- cross(ip-drop$O,ip)
    rpn  <- rpn * c(1/norm(rpn,type="2"))
    if(parameters$showRefractionPlane)
        scg[['refractionPlane']] <- plane(rpn,d = - rpn %*% ip, alpha=0.1)

    ## refract into drop
    o2i <- refract(ray,t,drop,dir="o2i")
    t <- max(unlist(intersect(o2i,drop)))
    scg[['rayIntoDrop']] <- getShape(o2i,t)
    ni = ni + 1

    ## do the reflections
    ir <- o2i
    for(i in 2:(maxInteractions-1))
    {
        rr <- reflect(ir,t,drop)
        i2i <- rr$ray
        if(is.null(i2i))
        {
            return(NULL)
        }
        t <- max(unlist(intersect(ir,drop)))
        name = paste("reflection",i-1,sep="")
        scg[[name]] <- getShape(i2i,t)
        rp <- point.ray(ir,t)
        nvR <- normal.drop(drop,rp)
        if(parameters$showNormals)
        {
            name <- paste(name,"normal",sep="")
            scg[[name]] <- arrow(rp,rp+nvR*drop$R,s=1/8,color="black")
        }

        ir <- i2i
        ni = ni + 1
    }

    ## refract out of ray
    i2o <- refract(ir,t,drop,dir="i2o")

    ep <- point.ray(ir,t)
    nvD2 <- normal.drop(drop,ep)
    if(parameters$showNormals)
        scg[['exitNormal']] <- arrow(ep,ep+nvD2*drop$R,s=1/8,color="black")



    ## exit angle
    angOut <- angle(CSYS,i2o$D)
    angD <- acos(ray$D %*% i2o$D)

    return(list('ray' = i2o, 'scg'= scg,
                'angIn' = angIn, 'angOut' = angOut, 'angR' = rr$thetaI,
                'angD' = angD, 'ni' = ni))
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

## follow a ray through the drop and
## let it continue for t units
## will build a scene graph
follow <- function(ray,drop,t=5000,nInt=3,id=0,parameters)
{
    s1 <- launch(ray,drop,maxInteractions=nInt,parameters)
    if(!is.null(s1))
    {
        name = paste('r',id,"fanOut",sep="")
        s1$scg[[name]] <- fanOut(s1$ray,t)
    }
    s1
}

sendLight <-function(rays,universe,observer,parameters)
{
    scgO <- sceneGraph()
    rayStats <- data.table(angIn=numeric(),
                           angOut=numeric(),
                           angRef=numeric(),
                           angD=numeric(),
                           ni=numeric(),
                           lambda=numeric(),
                           color=numeric())
    n = 0
    for( o in universe)
    {
        scgI <- sceneGraph()
        m = 0
        for( r in rays$chromaRays)
        {
            breakL = FALSE
            m = m + 1
            name = paste("r3",m,r$lambda,sep="")
            tryCatch(
            {
                I <- observer(r,o,
                              t=parameters$outRayLength,
                              nInt=parameters$nInteractions,id=0,parameters)
                scgI[[name]] <- I$scg
                dl <- list(I$angIn,I$angOut,I$angR,I$angD,I$ni,r$lambda,r$color)
                rayStats <- rbind(rayStats,dl)

            },
            error=function(cond)
            {
                warning("observer error")
                breakL = TRUE
            }
            )
          }
        n = n+1
        name = paste("uniObj",n,sep="")
        scgO[[name]] <- scgI
    }
    return(list(scg=scgO,rayData=rayStats))
}

