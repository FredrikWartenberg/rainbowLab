## Nicer Sphere for later
spheres3dS <- function(x0 = 0, y0 = 0, z0 = 0, r = 1, n = 101, ...){
  f <- function(s,t){
    cbind(   r * cos(t)*cos(s) + x0,
             r *        sin(s) + y0,
             r * sin(t)*cos(s) + z0)
  }
  persp3d(f, slim = c(-pi/2,pi/2), tlim = c(0, 2*pi), n = n, add = T, ...)
}


## simple scene
drop <-function(x=0,y=0,z=0,R=100,color="green",alpha="0.1",n=1.33)
{
    drop <- list('O'     =  c(x,y,z),
                 'R'     = R,
                 'color' = color,
                 'alpha' = alpha,
                 'n'     =  1,33)
    class(drop) <- "drop"
    return(drop)
}

render.drop <- function(drop)
{
     spheres3d(drop$O,r=drop$R,color=drop$color,alpha=drop$alpha)
}


## calculate the normal vector on
## the surface of the drop
normal.drop <-function(drop, point, accuracy = 1E-12)
{

    nv <- (point-drop$O)
    if(abs(norm(nv,type = "2") - drop$R)> accuracy)
    {
        warning("Point for normal caclulatrion not on surface of drop")
    }
    sf <- 1/norm(nv,type = "2")
    nv <-  nv * c(sf)

    return(nv)

}
