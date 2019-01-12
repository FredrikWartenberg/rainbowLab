## Libraries and includes
#' @import data.table
#' @import rgl
#' @import ggplot2


##' @title Retrieve default paramenters
##' @return list with default parameters
##' @author Fredrik Wartenberg
defaultParameters <-function()
{
    parameters <- list()

    ## defaults
    parameters[['nInteractions']] <- 3
    parameters[['outRayLength']] <- 200
    parameters[['showNormals']] <- FALSE
    parameters[['showRefractionPlane']] <- FALSE

    return(parameters)
}





