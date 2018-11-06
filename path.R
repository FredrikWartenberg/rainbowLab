launch <- function(ray,drop,maxInteractions=3,render=TRUE,debug=TRUE)
{

    browser()
    ## list of interactions
    ias <- list()

    ## first interaction is from out side to inside
    ias[[1]] <- list(ray,intersect(ray,drop))
    ## check if type of interaction is relevant
    ts <- unlist(ias[[1]][2])
    if(length(ts) != 2)
    {
        ## break as no or tangential interaction
        return(ias)
    }
    ## choose first intersection point
    t <- min(ts)
    if(render)
    {
        render(ray,t)
        iP <- point.ray(ray,t)
        if(debug)
        {
            ## render normal and refraction plane
            nv <- normal.drop(drop,iP)
            arrow3d(iP,iP+nv*drop$R,s=1/4,color="red")
        }
    }

    return(ias)
}
