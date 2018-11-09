
sceneGraph <- function()
{
    scg <- list()
    class(scg) <- "sceneGraph"
    return(scg)
}

sceneGraphPrune <- function(scg,exclude)
{

}


getShape <- function(object,t)
{
    UseMethod("getShape",object)
}

getShape.ray <- function(ray,t)
{
    arrow(point.ray(ray,0),
          point.ray(ray,t),
          s=1/20,
          color=ray$color)
}

render <- function(shape,t)
{
    UseMethod("render",shape)
}

render.default <- function(shape)
{
    print(paste("Render called for object of class", class(shape)))
}

render.sceneGraph <- function(sceneGraph)
{
    sapply(X=sceneGraph,FUN = render)
}

