constants <- function()
{
    list('c' = 3E8,
         'k' = 1.38E-23,
         'h' = 6.62E-34,
         'visibleMin' = 390,  ## nm
         'visibleMax' = 700   ## nm
         )
}

## units : 1 = 1mm
accuracy <-function()
{
    ##    1E-3 ## 1 uM around 1 lambda
    1E-3
}


## returns frequency in Hz for lambda
## lambda in nm
lambda2f <- function(lambda,pc=physConstants)
{
    pc$c/(lambda*1E-9)
}

## Plack's formula for distribution of
## Black Body Radiation
bbr <- function(f,T=6000,pc=physcialConstants)
{
    2* pc$h * f^3 / ( pc$c^2 * (exp(pc$h*f/pc$k/T)-1))
}
bbx <- function(x){bbr(x,6000,physConstants)}
## Plot BBR
##curve(bbx,from=1,to=1E15)

##     This converts a given wavelength of light to an
##     approximate RGB color value. The wavelength must be given
##     in nanometers in the range from 380 nm through 750 nm
##     (789 THz through 400 THz).

##     Based on code by Dan Bruton
##     http://www.physics.sfasu.edu/astro/color/spectra.html
##     and
##     http://www.noah.org/wiki/Wavelength_to_RGB_in_Python

lambda2rgb <-function (wavelength, alpha= 1, gamma=1)
{
    if (wavelength >= 380 & wavelength <= 440)
    {
        attenuation = 0.3 + 0.7 * (wavelength - 380) / (440 - 380)
        R = ((-(wavelength - 440) / (440 - 380)) * attenuation) ** gamma
        G = 0.0
        B = (1.0 * attenuation) ** gamma
    }
    else if(wavelength >= 440 & wavelength <= 490)
    {
        R = 0.0
        G = ((wavelength - 440) / (490 - 440)) ** gamma
        B = 1.0
    }
    else if(wavelength >= 490 & wavelength <= 510)
    {
        R = 0.0
        G = 1.0
        B = (-(wavelength - 510) / (510 - 490)) ** gamma
    }
    else if( wavelength >= 510 & wavelength <= 580)
    {
        R = ((wavelength - 510) / (580 - 510)) ** gamma
        G = 1.0
        B = 0.0
    }
    else if( wavelength >= 580 & wavelength <= 645)
    {
        R = 1.0
        G = (-(wavelength - 645) / (645 - 580)) ** gamma
        B = 0.0
    }
    else if( wavelength >= 645 & wavelength <= 750)
    {
        attenuation = 0.3 + 0.7 * (750 - wavelength) / (750 - 645)
        R = (1.0 * attenuation) ** gamma
        G = 0.0
        B = 0.0
    }
    else
    {
        R = 0.0
        G = 0.0
        B = 0.0
    }

    R = round(R * 255)
    G = round(G * 255)
    B = round(B * 255)

    return(rgb(R,G,B,alpha=alpha,maxColorValue = 255))
}


## Segelstein, D., 1981: "The Complex Refractive Index of Water",
## M.S. Thesis, University of Missouri--Kansas City
## http://www.philiplaven.com/Segelstein.txt
## refered to from
## http://www.philiplaven.com/p20.html

## returns a function whicht maps
## lambda in nm to refractive index
## for water
refractiveIndexFnGenerator <- function(datafile,lambda)
{
    ## read data
    riTab <- fread(datafile)

    ## Generate function to return
    ##function(lambda)
    ##{
        ##riTab[abs(Wavelength * 1000 - lambda) < 1]$"Real Index"
    ##}
    approxfun(riTab$Wavelength*1000,riTab$"Real Index")
}

## The actual function to avoid table lookup
refractiveIndex <- refractiveIndexFnGenerator("Segelstein.txt")

## Light spectra
uniformSpectrum <- function(from = constants()$visibleMin,
                            to =   constants()$visibleMax,
                            steps = 20) ## FIX
{
    round(
        seq(from = constants()$visibleMin,
        to = constants()$visibleMax,
        length.out=steps))
}


## Light sources

## Ray light will emit a singlel ray
## along <ray>
## with different wavelengths
## as defined by spectrum
rayLight <- function(...)
{

    rl <- list()
    rl[['rayLightRay']]  <- ray(...)
    class(rl) <- "rayLight"

    return(rl)
}

## Applies spectrum to a list of rays
## for every inbound ray one ray with
## the same direction is generated for
## all spectral components
spectralize <- function(rays,spectrum=uniformSpectrum())
{
    makeRay <- function(lambda,ray)
    {
        ray$lambda = lambda
        ray$color  = lambda2rgb(lambda)
        return(ray)
    }

    chromaRays <- list()
    n=0
    for(r in rays)
    {
        for(l in spectrum)
        {
            n= n+1
            name = paste("n",n,"l",l,sep="")
            chromaRays[[name]] <- makeRay(l,r)
        }
    }

    rl <- list('chromaRays' = chromaRays, 'inRrays' = rays)
    class(rl) <- "spectralRays"
    return(rl)
}

## arc light will emit a group of identical rays
## along an arc
## with different wavelengths
## as defined by spectrum
arcLight <- function(focus     = c(0,0,0),
                     arcPoint  = c(400,0,0),
                     normalAxis = c(0,1,0),
                     origin = c(0,0,0),
                     toAngle = pi/3,
                     fromAngle   = -pi/3,
                     steps     = 10,
                     renderLength = 100)
{

    ## reverse rays
    rr <- function(ray)
    {
        rr <- ray
        rr$O = point.ray(ray,t=renderLength)
        rr$D = - c(ray$D)
        return(rr)
    }

    ## generate angles
    angs <- seq(fromAngle,toAngle,length=steps)

    ## generate arcPoints
    rays <- list()
    n=0 ## Fix
    for(a in angs)
    {
        n = n + 1
        rm <- rotationMatrix(normalAxis,a)
        r <- ray(O=c(0,0,0),D = -arcPoint %*% rm)
        rays[[as.character(a)]] <- rr(r)
    }

    class(rays) <- "arcLight"
    return(rays)
}

## line light will emit a group of identical rays
## along a line
lineLight <- function(D       = c(1,0,0),
                      origin  = c(-100,10,0),
                      end     = c(-100,200,0),               ## FIX
                      steps   = 10,
                      renderLength = 100)
{

    ## generate ray light points
    dVector <- (end - origin)/steps
    rayOs <- lapply(0:steps,FUN = function(x){dVector*x})
    rays <- lapply(rayOs,FUN = function(x){ray(O=x,D=D)})

    class(rays) <- "lineLight"
    return(rays)
}
