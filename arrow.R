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
    arrow3d(p0 = arrow$p0,
            p1 = arrow$p1,
            s  = arrow$s,
            color=arrow$color)
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



