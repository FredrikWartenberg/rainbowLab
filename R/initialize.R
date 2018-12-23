## Libraries and includes
## library(rgl)
## library(data.table)
## library(pracma)
## library(magrittr)
## library(listviewer)
## library(ggplot2)

## source("R/drop.R")
## source("R/ray.R")
## source("R/shapes.R")
## source("R/path.R")
## source("R/physics.R")
## source("R/maths.R")
## source("R/plot.R")
## source("R/generateRainbows.R")

#' @import data.table
#' @import rgl


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





