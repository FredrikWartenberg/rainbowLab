render <- function(shape)
{
    UseMethod("render",shape)
}

render.default <- function(shape)
{
    print(paste("Render called for object of class", class(shape)))
}

