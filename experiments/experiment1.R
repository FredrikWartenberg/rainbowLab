## Experiment 1
source("./scene.R")

## details on observer
## observer <- function(ray,drop)
## {
##     follow(ray,drop,nInt=parameters$nInteractions,t=parameters$outRayLength)
## }

##observer <- follow

## Line light new style
##lineL <- lineLight(steps=100,origin=c(-400,10,0),end=c(-400,370,0))  ## returns a list of rays, can be plotted
lineL <- arcLight(arcPoint=c(-400,0,0),steps=100,fromAngle = 0,toAngle = pi/3,focus=c(-200,0,0),debug=TRUE)  ## returns a list of rays, can be plotted
fullScene[['ll']]  <- lineL ## Add to scene graph for plotting
lineLFollow <- lineL %>% spectralize(spectrum=monochromaticSpectrum()) %>% sendLight(univ,follow,parameters)
##lineLFollow <- lineL  %>% sendLight(univ,follow,parameters)

## result to rendering
fullScene[['lLF']] <- lineLFollow$scg

## result to datab
pd <- prepareData(lineLFollow$rayData)

## Plots

plot2 <- function(pd)
{
    p <- (
        ggplot(data=pd)
        + geom_point(
              aes(x=lambda,y=180+(angOut)*180/pi ,color = color)
          )
        + facet_grid(.~angInDeg)
        + theme(legend.title=element_blank())
        + labs(title="Deflection Angles over Incident Angle and Lambda")
        + theme(legend.title=element_blank())

    )

    print(p)

}

plot2(pd)

## render
render(fullScene)
