
sceneGraph <- function()
{
    scg <- list()
    class(scg) <- "sceneGraph"
    return(scg)
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

