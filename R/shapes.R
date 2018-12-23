## ##########################################
## Functions for rendering

##' Render scene
##'
##' Scene is a the sceneGraph representation of the scene.
##' render will tranverse the scene graph and call the
##' render functions of the various objects.
##' It will do so by calling the generic render function.
##' rgl is a buggy library, so expect surprises
##' @title Render Scene
##' @param scene scenGraph
##' @return nothing
##' @author Fredrik Wartenberg
##' @export
renderScene<-function(scene)
{
    render(scene)
}


## Generic render function
##' Generic render function
##'
##' Will dispatch to object's render functions
##' @title Dispatch funtction for rendering objects
##' @param shape object to render
##' @param t unclaer
##' @return nothing
##' @author Fredrik Wartenberg
##' @export
render <- function(shape,t)
{
    UseMethod("render",shape)
}

##' @export
render.default <- function(shape)
{
    print(paste("Render called for object of class", class(shape)))
}


## Scene graph to render
sceneGraph <- function()
{
    scg <- list()
    class(scg) <- "sceneGraph"
    return(scg)
}

##' @export
render.sceneGraph <- function(sceneGraph)
{
    sapply(X=sceneGraph,FUN = render)
}

universe <- function()
{
    uni <- list()
    class(uni) <- "universe"
    return(uni)
}

##' @export
render.universe <-function(uni)
{
    sapply(X=uni,FUN = render)
}

## map objects to shapes
getShape <- function(object,t)
{
    UseMethod("getShape",object)
}

## rays
getShape.ray <- function(ray,t)
{

    arrow(point.ray(ray,0),
          point.ray(ray,t),
          s= ray$s,
          color=ray$color)
}

## Functions related to shape rendering
## if s = 0 arrow is rendered as a line
arrow <-function(p0=c(0,0,0),p1=c(1,0,0),s=1/20,color="yellow")
{
    arrow <- list('p0'     =  p0,
                  'p1'     = p1,
                  'color' = color,
                  's' = s)
    class(arrow) <- "arrow"
    return(arrow)
}

##' @export
render.arrow <- function(arrow)
{
##    if(arrow$s != 0)
    if(TRUE)
    {
    arrow3d(p0 = arrow$p0,
            p1 = arrow$p1,
            s  = 1/20,##arrow$s,
            type = "lines",
            color=arrow$color)
    }else
    {
        cds <-  rbind(arrow$p0,arrow$p1)
        lines3d(x=cds[,1],
                y=cds[,2],
                z=cds[,3],
                color=arrow$color)
    }
}

plane <-function(a=c(1,0,0),d=0,alpha=0.1,color="yellow")
{
    plane <- list('a'     =  a,
                  'd'     = d,
                  'color' = color,
                  'alpha' = alpha)
    class(plane) <- "plane"
    return(plane)
}

##' @export
render.plane <- function(plane)
{
    planes3d(a=plane$a,
             d =     plane$d,
             alpha = plane$alpha,
             color = plane$color)
}

##' @export
render.drop <- function(drop)
{
     spheres3d(drop$O,r=drop$R,color=drop$color,alpha=drop$alpha)
}

##' @export
render.rayLight <- function(rayLight)
{
    ##ray <- rayLight$ray
    ray <- rayLight[[1]]

    arrow3d(p0 = ray$O,
            p1 = point.ray(ray,t=100),
            s  = 1/10,,
            color="white")
}

##' @export
render.arcLight <- function(arcLight)
{
    getAndRender <- function(ray)
    {
        render(getShape(ray,t=100))
    }
    lapply(arcLight,FUN=getAndRender)
}


##' @export
render.lineLight <- function(lineLight)
{
    getAndRender <- function(ray)
    {
        render(getShape(ray,t=100))
    }
    lapply(lineLight,FUN=getAndRender)
}


## Nicer Sphere for later
spheres3dS <- function(x0 = 0, y0 = 0, z0 = 0, r = 1, n = 101, ...){
  f <- function(s,t){
    cbind(   r * cos(t)*cos(s) + x0,
             r *        sin(s) + y0,
             r * sin(t)*cos(s) + z0)
  }
  persp3d(f, slim = c(-pi/2,pi/2), tlim = c(0, 2*pi), n = n, add = T, ...)
}

## Geomoetrical functions/sh
intersect <- function(shape,oShape)
{
    UseMethod("intersect",shape)
}

intersect.default <- function(shape,oShape)
{
    print(paste("Intersect called for object of class", class(shape)))
}


## Return scene graph for Coordinate system
coordSys <- function(axLen = 500)
{

    scg <- sceneGraph()
    if(axLen != 0)
    {
        scg[['x']] <- arrow(p0=c(-axLen,0,0),p1=c(axLen,0,0),s=1/20,color="red")
        scg[['y']] <- arrow(p0=c(0,-axLen,0),p1=c(0,axLen,0),s=1/20,color="green")
        scg[['z']] <- arrow(p0=c(0,0,-axLen),p1=c(0,0,axLen),s=1/20,color="black")
    }
    return(scg)
}


##' The water drop through which rays are traced
##'
##' Contains both physical and graphical parameters for rendering.
##' @title water drop
##' @param x x-coordinate of centre
##' @param y y-coordinate of centre
##' @param z z-coordinate of centre
##' @param R radius
##' @param color rendering color
##' @param alpha rendring alpha
##' @param n refractive index (default)
##' @param ri function returning refractive index
##' @return drop objecr
##' @author Fredrik Wartenberg
##' @export
drop <- function(x=0,y=0,z=0,R=100,color="green",alpha="0.1",n=1.33,ri=refractiveIndex)
{
    drop <- list('O'     =  c(x,y,z),
                 'R'     = R,
                 'color' = color,
                 'alpha' = alpha,
                 'ri' = ri,
                 'n'     =  n)
    class(drop) <- "drop"
    return(drop)
}


## calculate the normal vector on
## the surface of the drop
normal.drop <-function(drop, point)
{

    nv <- (point-drop$O)
    if(abs(norm(nv,type = "2") - drop$R)> accuracy())
    {
        warning("Point for normal calculatrion not on surface of drop")
    }
    sf <- 1/norm(nv,type = "2")
    nv <-  nv * c(sf)

    return(as.vector(nv))

}


