## User interface DSL functions for rainbowLab

## initialize
source("initialize.R")

## ######################################
## Physics functions and data
## ######################################

## Print the physical constants
physicalConstants <- function()
{
    cat("Speed of Light        c [m/s]         =",constants()$c,"\n")
    cat("Boltzmann's constant  k [J/K]         =",constants()$k,"\n")
    cat("Planck's constant     h [Js]          =",constants()$h,"\n")
    cat("Visible Light minimum wavelength [nm] =",constants()$visibleMin,"\n")
    cat("Visible Light maximum wavelength [nm] =",constants()$visibleMax,"\n")
}

## Plot Refractive Index
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

## Raytrace a single ray in a number of wavelengths
## The drop is located at x = 400 and has a radius of 400
## The light source is one single reay
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


## Generate the rainbow distributions
## No 3D rendering
generateRainbows <-function(rainbows=c(1,2),nColors = 5, resolution = 1)
{

    ## universe containing the interacting drop
    univ <- universe()
    univ[['d1']] <- drop(x=400,R=400, color="blue")

    ## Generate the data
    angleSteps=90/resolution
    pd <- generateDataMatrix(universe=univ,
                       angularSteps=angleSteps,
                       lambdaSteps = nColors,
                       rainbows=rainbows)
    ## Plots
    ## Plots
    windows()
    ##plotPDF(pd)
    plotMaxima(pd)

    windows()
    plotPDFLines(pd)

    ## Return data
    return(pd)
}
