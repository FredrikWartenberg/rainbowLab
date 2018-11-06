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
    arrow3d(p1 = arrow$p0,
            p0 = arrow$p1,
            s  = arrow$s,
            color=arrow$color)
}


