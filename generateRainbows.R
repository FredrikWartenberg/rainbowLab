## Function to generate rainbow data
prepareData <- function(pd,simplify=FALSE)
{
    pd$angInDeg  <- pd$angIn*180/pi
    pd$angOutDeg <- pd$angOut*180/pi
    pd$angRefDeg <- pd$angRef*180/pi+180
    pd$angD2Deg  <- pd$angD*180/pi
    pd$angDDeg   <- 180-pd$angD2Deg
    ##pd[ni > 4, angDDeg := angD2Deg]
    pd$rainbowNo <- as.factor(pd$ni - 2)
    ##pd$intensity <- 1
    pd$angD2Deg <- NULL

    ## remove radian fields
    if(simplify)
    {
        pd$angIn <- NULL
        pd$angOut <- NULL
        pd$angRef <- NULL
        pd$angD <- NULL
        pd$ni <- NULL
    }

    return(pd)
}

aggregateData <- function(dm)
{
    ##dm     <- dm[angDDeg<60]
    browser()
    dm$bin <- cut(dm$angDDeg,breaks=200)
    dm[,intensity:=calculateIntensity(lambda,angIn,angOut,angRef,ni-2)]
    dmAgg  <- dm[,
                 .(.N,I=sum(intensity),angD=mean(angDDeg)),
                 by=.(rainbowNo,bin,lambda)]
    return(dmAgg)
}

## apply schlicks approximation
## for the ray propagation in the drop
calculateIntensity <- function(lambda,thetaI,thetaE,thetaR,nR,method="none")
{
    ## Prepare
    n = refractiveIndex(lambda)
    I = 1

    if(method == "schlick")
    {

        ## Entry
        I = I * (1-schlick(thetaI,n))
        ## Internal Reflection
        I = I * schlick(thetaR,n)^as.numeric(nR)
        ## Exit
        I = I * (1-schlick(thetaE,n))

    } else if (method == "fresnel")
    {
##        I = fresnel(theta
    }

    return(I)
}

## Schlick's approximation for the reflection coefficient
schlick <-function(theta,n=1.34)
{
    R_0  = ((n-1)/(n+1))^2
    R    = R_0 + (1-R_0)*(1-abs(cos(theta)))^5
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

## find the maxima of the distributions for color x nRainbow
## returns a table with maximum & angle for lambda x rainbowNo
maxima <- function(dm)
{
    ## Aggregate and bin data
    dm <- aggregateData(dm)
    dm$binN <- as.numeric(dm$bin)
    keycols=c("rainbowNo","lambda","N")
    setkeyv(dm,keycols)

    dmM <- dm[,.(max=max(N)),by=.(rainbowNo,lambda)]
    keycols=c("rainbowNo","lambda","max")
    setkeyv(dmM,keycols)

    return(dm[dmM])
}


