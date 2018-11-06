## render <- function(shape,t)
## {
##     UseMethod("render",shape)
## }

## render.default <- function(shape,t)
## {
##     print(paste("Render called for object of class", class(shape)))
## }


intersect <- function(shape,oShape)
{
    UseMethod("intersect",shape)
}

intersect.default <- function(shape,oShape)
{
    print(paste("Intersect called for object of class", class(shape)))
}

