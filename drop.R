## simple scene
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

## A screen to catch rays and show them
screen <- function(origin=c(0,0,0),normal=c(1,0,0),color="white",alpha=0.1)
{

}

