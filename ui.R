## User interface DSL functions for rainbowLab

## initialize
source("initialize.R")

## ######################################
## High Level UI functions
## ######################################

##' Print the physical constants used
##'
##' to retrieve constants use constants() function
##' @title Print Physical constants
##' @return nothing
##' @author Fredrik Wartenberg
physicalConstants <- function()
{
    cat("Speed of Light        c [m/s]         =",constants()$c,"\n")
    cat("Boltzmann's constant  k [J/K]         =",constants()$k,"\n")
    cat("Planck's constant     h [Js]          =",constants()$h,"\n")
    cat("Visible Light minimum wavelength [nm] =",constants()$visibleMin,"\n")
    cat("Visible Light maximum wavelength [nm] =",constants()$visibleMax,"\n")
}

##' Plot Refractive Index
##'
##' Prints a color coded plot of the refractive index over lambda
##' @title Plot Refractive Index
##' @return nothing
##' @author Fredrik Wartenberg
plotRefractiveIndex <- function()
{
    ## generate data table
    lambda = seq(from   = constants()$visibleMin,
                 to     = constants()$visibleMax,
                 length = 100)
    data <- data.table(lambda)
    data$refractiveIndex <- refractiveIndex(data$lambda)
    col <- sapply(FUN=lambda2rgb,X=data$lambda)
    data$color <- col

    ## compose plot
    p <-
        (
            ggplot(data=data)
            + ggtitle(
                  paste("Refractive Index for Water for Visible Light",
                        "Source: Segelstein 1981",
                        "The Complex Refractive Index of Water",sep="\n"))
            ##+ scale_x_discrete(name="lambda [nm]")
            + geom_point(aes(x=lambda,
                             y=refractiveIndex,
                             color=color),
                         show.legend=FALSE)


        )

    ## display
    print(p)

}

##' Trace and spectralize one ray and show it in a 3D plot
##'
##' The drop is located at x = 400 and has a radius of 400
##' The light source is one single reay
##' @title Trace single ray
##' @param nColors number of colors from the uniform spectrum. Default = 10.
##' @param nInteractions interactions of the ray with the drop. nInteractions -2 gives rainbow number. Default = 3.
##' @param showNormals if TRUE, show normals in the interaction point anp plane of interaction. Default = FALSE.
##' @return Nothing
##' @author Fredrik Wartenberg
traceOneRay <- function(nColors=10,nInteractions=3,showNormals=FALSE)
{
    ## Define basic scene

    ## Define display window
    ## rgl is a really buggy library
    clear3d(type="all")                        ## reset
    open3d(windowRect = c(100,100,1000,1000))  ## window size
    bg3d(color=c("#664455"))                   ## Background color

    ## Build the static background scene
    fullScene <- sceneGraph()
    fullScene[['coordSys']] <- coordSys(1000) # add coordinate system

    ## universe of interacting objects
    ## Here just one drop
    univ <- universe()
    univ[['d1']] <- drop(x=400,R=400, color="blue")
    fullScene[['univ']] <- univ ## add universe to scene

    ## #########################################################
    ## parameters and settings
    parameters <- defaultParameters()

    ## override
    parameters[['outRayLength']] <- 1000
    parameters[['nInteractions']] <- nInteractions
    parameters[['showNormals']] <- showNormals
    parameters[['showRefractionPlane']] <- showNormals

    ## #########################################################
    ## send the light

    ## Define a single light ray
    lightRay <- rayLight(O=c(-400,380,0),D=c(1,0,0))
    ## Add ray to scene for rendering
    fullScene[['ll']]  <- lightRay

    ## apply spectrum to the ray
    ## for each wavelength one ray is generated
    ## parallel to the original ray
    ## here we use a unform spectrum with nSteps
    ## light source with lambda = 400 nm
    spectrum = uniformSpectrum(steps=nColors)
    spectrumRays <- spectralize(lightRay,spectrum)

    ## now send the light through the universe (= light drop)
    tracedRays <- sendLight(spectrumRays,univ,follow,parameters)

    ## add to scene for rendering
    fullScene[['tracedRays']] <- tracedRays$scg

    ## render
    renderScene(fullScene)

    ## Retrun ray data
    return(prepareData(tracedRays$rayData,simplify=TRUE))

}

##' Generate the rainbow distributions and plot distributions and maxima.
##'
##' Will generate a table of traced rays for the selected rainbows and colors.
##' Rays with uniformly distributed angles between 0 and 90 degrees will traced through
##' a water drop the resolution specificies the angular steps.
##' Each ray will be spectralized with a uniformly distributed light spectrum
##' of nColors steps bewteen visbleMin and visbleMax. The function will not perform 3D rendering.
##' @title Generate Rainbows
##' @param rainbows The raibows to be generated. Specified by a vector with the rainbow number.
##' Example: c(1,2,3) will generate the first three rainbows. Default = c(1,2).
##' @param nColors Number of spectral colors. Default = 5.
##' @param resolution The angular resolution. Default = 1.
##' will generate 90 steps with 1 degree bewteen each step.
##' @return A data.table with one row for each colored ray. See generateDataMatrix() for details.
##' @author Fredrik Wartenberg
generateRainbows <-function(rainbows=c(1,2),nColors = 5, resolution = 1)
{

    ## universe containing the interacting drop
    univ <- universe()
    univ[['d1']] <- drop(x=400,R=400, color="blue")

    ## Generate the data
    angleSteps=90/resolution
    rayData <- generateDataMatrix(universe=univ,
                                  angularSteps=angleSteps,
                                  lambdaSteps = nColors,
                                  rainbows=rainbows)
    ## Plots
    windows()
    plotMaxima(rayData)

    windows()
    plotPDFLines(rayData)

    ## Return data
    invisible(rayData)
}
