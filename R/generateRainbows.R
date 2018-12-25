
##' Add fields in degrees and intensity to raytracing data
##'
##' Intensity is calculated according to intensity method. Currently
##' supported methods are fresnel (default) and identity
##' @title Prepare rayTracingData for analysis and plotting
##' @param rayTracingData data.table with raytracing data, one row per ray
##' @param simplify will remove radian fields if TRUE
##' @param intensityMethod Method to calculate intensity, see details
##' @return data.table with added fields, now called rayData
##' @author Fredrik Wartenberg
prepareData <- function(rayTracingData,simplify=FALSE,intensityMethod="fresnel")
{
    ## Add fields in degrees
    rayTracingData$angInDeg  <- rayTracingData$angIn*180/pi
    rayTracingData$angOutDeg <- rayTracingData$angOut*180/pi
    rayTracingData$angRefDeg <- rayTracingData$angRef*180/pi+180
    rayTracingData$angD2Deg  <- rayTracingData$angD*180/pi
    rayTracingData$angDDeg   <- 180-rayTracingData$angD2Deg
    rayTracingData$angD2Deg  <- NULL

    ## Rainbow Number
    rayTracingData$rainbowNo <- as.factor(rayTracingData$ni - 2)

    ## Add intesity per ray
    rayTracingData[,intensity:=
            calculateIntensity(lambda,angIn,angOut,angRef,ni-2,intensityMethod)
            ]

    ## add Max intensity per group for 3db cutoff
    ## rayTracingData[,maxI := max(intensity),by=.(rainbowNo,lambda)]

    ## remove radian fields
    if(simplify)
    {
        rayTracingData$angIn <- NULL
        rayTracingData$angOut <- NULL
        rayTracingData$angRef <- NULL
        rayTracingData$angD <- NULL
        rayTracingData$ni <- NULL
    }

    ## return transformed table, now actually rayData
    attr(rayTracingData,"intensityMethod") <- intensityMethod
    return(rayTracingData)
}



## apply schlicks approximation
## for the ray propagation in the drop
##' Calculate change in intensity of ray propagating through the water drop
##'
##' The identity method set attentuation to 1 (no attenuation)
##' The fresnel method calculates attenuation based on reflection
##' and transmission coefficients.
##' @title Intensity change
##' @param lambda Wavelength nm
##' @param thetaI Incident angle of ray into drop
##' @param thetaE excident angle of ray from drop
##' @param thetaR reflection angle inside drop
##' @param nR number of reflections
##' @param method intensity calculation method (see details=
##' @return attenuation value in linear domain (0..1)
##' @author Fredrik Wartenberg
calculateIntensity <- function(lambda,thetaI,thetaE,thetaR,nR,method="identity")
{
    ## Prepare
    n = refractiveIndex(lambda)


    if(method == "schlick"){

        stop(paste("Intesity method no longer supported:", method))
        ## Entry
        I = I * (1-schlick(thetaI,n))
        ## Internal Reflection
        I = I * schlick(thetaR,n)^as.numeric(nR)
        ## Exit
        I = I * (1-schlick(thetaE,n))

    } else if (method == "fresnel"){

        ## we use _m; average over s and p polarisation
        ## Entry, consider transmission
        I = unlist(mapply(FUN = fresnel,theta = thetaI,n=n,dir = "o2i")["T_m",])
        ## Internal Reflection, consider reflection
        IR <- unlist(mapply(FUN = fresnel,theta = thetaR,n=n,dir = "i2o")["R_m",])
        I = I * (IR ^ nR)
        ## Exit, consider transmision
        I = I * unlist(mapply(FUN = fresnel,theta = thetaR,n=n,dir = "i2o")["T_m",])
    } else if (method == "identity"){
        ## do nothing
        I = 1
    } else {
        stop(paste("Unkown intesity method:", method))
    }

    return(I)
}

##' Aggregate rayData to pdf
##'
##' Aggregates rayData over a range of angles. The range is implicitly
##' set by the number of bins. Cutoff removes values smaller than
##' a portion of max vaule
##' @title aggregate rayData
##' @param rayData data.table with rayData
##' @param nBreaks number of bins to aggregate into
##' @param cutoff exclude rows which are smaller than max*cutoff (0 = no cutoff)
##' @return data.table with pdf of angular difference (angDDeg)
##' @author Fredrik Wartenberg
aggregateData <- function(rayData,nBreaks=200,cutoff=0.5)
{
    rayData$bin <- cut(rayData$angDDeg,breaks=nBreaks)

    pdfData  <- rayData[!is.na(intensity),
                 .(.N,I=sum(intensity),angD=mean(angDDeg)),
                 by=.(rainbowNo,bin,lambda)]

    ## cutoff
    pdfData[,maxI := max(I),by=.(rainbowNo,lambda)]
    pdfData[I < maxI*cutoff, I := 0]

    ## inherit intesity method
    attr(pdfData,"intensityMethod") <- attr(rayData,"intensityMethod")

    return(pdfData)
}


##' Maxima (Peaks) of pdf
##'
##' find the maxima of the distributions for color x nRainbow
##' returns a table with maximum & angle for lambda x rainbowNo
##' Fresnel intensity method is biased towards too small angels.
##' @title Find angels of mximum intensity
##' @param pdfData data.table with pdf (= aggregated ray data)
##' @return table with maximum & angle for lambda x rainbowNo
##' @author Fredrik Wartenberg
##' @export
maxima <- function(pdfData)
{

    keycols=c("rainbowNo","lambda","I")
    setkeyv(pdfData,keycols)
    pdfDataMax <- pdfData[,.(max=max(I)),by=.(rainbowNo,lambda)]

    keycols=c("rainbowNo","lambda","max")
    setkeyv(pdfDataMax,keycols)

    return(pdfData[pdfDataMax])
}



##' Generates the table with spectral and angular distribution of the different rainbows.
##'
##' Central simulation function
##' @title Generate Rainbow Spectral Distributions
##' @param universe The universe (typically one drop)
##' @param angularSteps Angular resolution for simulation steps
##' @param lambdaSteps Stepsize for lambda between visibleMin and visibleMax
##' @param rainbows Rainbows to generate, e.g. c(1,2,4) for first, second and fourth rainbow
##' @param intensity How to calculate the intensity in the spectrum:
##' count: just count the number of rays
##' fresnel: apply fresenel formulas to transmission and reflectio
##' @return a data.table with the spectral and angular intesities for each rainbow
##' @author Fredrik Wartenberg
generateDataMatrix <- function(
                               universe,
                               angularSteps = 10000,
                               lambdaSteps = 1,
                               rainbows = c(1,2),
                               intensity = "count")
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
        rayData <- prepareData(tracedRays$rayData)
        dataMatrix <- rbind(dataMatrix,rayData)
    }

    ## return data silently (now rayData
    invisible(dataMatrix)
}
