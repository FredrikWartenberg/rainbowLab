## Generic render function
render <- function(shape,t)
{
    UseMethod("render",shape)
}

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

render.sceneGraph <- function(sceneGraph)
{
    ##save <- par3d(skipRedraw=TRUE)
    ##on.exit(par3d(save))
    sapply(X=sceneGraph,FUN = render)
}


sceneGraphPrune <- function(scg,exclude)
{

}

universe <- function()
{
    uni <- list()
    class(uni) <- "universe"
    return(uni)
}

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

render.plane <- function(plane)
{
    planes3d(a=plane$a,
             d =     plane$d,
             alpha = plane$alpha,
             color = plane$color)
}


render.drop <- function(drop)
{
     spheres3d(drop$O,r=drop$R,color=drop$color,alpha=drop$alpha)
}

render.rayLight <- function(rayLight)
{
    ##ray <- rayLight$ray
    ray <- rayLight[[1]]

    arrow3d(p0 = ray$O,
            p1 = point.ray(ray,t=100),
            s  = 1/10,,
            color="white")
}

render.arcLight <- function(arcLight)
{
    getAndRender <- function(ray)
    {
        render(getShape(ray,t=100))
    }
    lapply(arcLight,FUN=getAndRender)
}


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

## generate a line of drops
dropLine <- function(O=c(0,-500,0),D=c(0,1,0),R=400,d=100,n=5)
{
    scg <- sceneGraph()
    D <- D/norm(D,type="2") # normalize direction
}
