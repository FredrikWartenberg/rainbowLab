## Function to generate rainbow data
prepareData <- function(pd)
{
    pd$angInDeg <- round(pd$angIn*180/pi,3)
    pd$angOutDeg <- round(pd$angOut*180/pi,3)
    pd$angRefDeg <- round(pd$angRef*180/pi+180,3)
    pd$angD2Deg <- round(pd$angD*180/pi,3)
    pd$angD2DegCorr <- 180-pd$angD2Deg
    pd[ni > 4, angD2DegCorr := angD2Deg]
    pd$rainbowNo <- as.factor(pd$ni - 2)
    ##pd[angDiffDeg > 180,angDiffDeg:= angDiffDeg -180]
    return(pd)
}


generateDataMatrix <- function(
                               universe,
                               angularSteps = 10000,
                               lambdaSteps = 1,
                               rainbows = c(1,2))
{
    parameters <- defaultParameters()

    ## Define light rays with angular distribution
    lightRays <- arcLight(fromAngle=0,toAngle=pi/2,steps=angularSteps)

    ## Apply spectrum
    spectrumRays <- spectralize(lightRays,
                                spectrum=uniformSpectrum(steps=lambdaSteps))

    ## Generate data for rainbows
    dataMatrix <- NULL
    for(i in rainbows)
    {

        ## announce
        cat(paste("Generating Rainbow #", i, "of", length(rainbows), "\n"))

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

