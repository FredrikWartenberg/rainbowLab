## Experiment 1
source("./scene.R")

## details on observer
## observer <- function(ray,drop)
## {
##     follow(ray,drop,nInt=parameters$nInteractions,t=parameters$outRayLength)
## }

##observer <- follow

parameters[['showNormals']] <- TRUE
parameters[['showRefractionPlane']] <- TRUE


lightRay <- rayLight(O=c(-400,300,0),D=c(1,0,0))         ## Define Light ray
fullScene[['ll']]  <- lightRay                          ## Add to scene graph for plotting
rayFollow <- lightRay %>% spectralize(spectrum=monochromaticSpectrum(400)) %>% sendLight(univ,follow,parameters)

## result to rendering
fullScene[['lLF']] <- rayFollow$scg

## result to datab
pd <- prepareData(rayFollow$rayData)
fwrite(file="experiments/singleRayWithDebug.csv",pd)
## Plots

plot2 <- function(pd)
{
    p <- (
        ggplot(data=pd)
        + geom_point(
              aes(x=lambda,y=180 - (-angIn + angOut)*180/pi ,color = color),
              size = 7
          )
        + labs(title="Deflection Angles over Incident Angle and Lambda")

    )

    print(p)

}

plot2(pd)

## render
render(fullScene)
