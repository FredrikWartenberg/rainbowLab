## Experiment for single ray
## with debug inof
source("./scene.R")

## Turn on ray nromals and planes
parameters[['showNormals']] <- FALSE
parameters[['showRefractionPlane']] <- FALSE
parameters[['nInteractions']] <- 3


generateDataMatrix <- function(
                               universe,
                               angularSteps = 10000,
                               lambdaSteps = 1,
                               maxRainbow = 1)
{
    ## Define light rays with angular distribution
    lightRays <- arcLight(fromAngle=0,toAngle=pi/2,steps=angularSteps)

    ## Apply spectrum
    spectrumRays <- spectralize(lightRays,
                                spectrum=uniformSpectrum(steps=lambdaSteps))

    ## Generate data for rainbows
    dataMatrix <- NULL
    for(i in 1:maxRainbow)
    {

        ## announce
        cat(paste("Generating Rainbow", i, "/", maxRainbow, "\n"))

        ## raytracing
        parameters[['nInteractions']] <- i + 2
        tracedRays <- sendLight(spectrumRays,universe,follow,parameters)

        ## retrieve and append data
        pd <- prepareData(tracedRays$rayData)
        dataMatrix <- rbind(dataMatrix,pd)
    }

    ## prepare for plotting
    dataMatrix <- prepareData(dataMatrix)
    return(dataMatrix)
}


## generate Data
pd <- generateDataMatrix(univ)

## Plots
plotPDF(pd)

windows()
plotInVsOut(pd)
##
