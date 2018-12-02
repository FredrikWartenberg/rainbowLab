## Libraries and includes
library(rgl)
library(data.table)
library(pracma)
library(magrittr)
library(listviewer)

source("drop.R")
source("ray.R")
source("shapes.R")
source("path.R")
source("physics.R")
source("maths.R")
source("plot.R")
source("generateRainbows.R")

## render scene
renderScene<-function(scene)
{
    render(scene)
}


## Define display window
##clear3d(type="all")
##open3d(windowRect = c(100,100,1000,1000))
##bg3d(color=c("#664455"))

## ## Build the static background scene
## fullScene <- sceneGraph()
## fullScene[['coordSys']] <- coordSys() # add coordinate system

## ## universe of interacting objects
## univ <- universe()
## univ[['d1']] <- drop(x=401,R=400, color="blue")
## fullScene[['univ']] <- univ ## add universe to scene

## parameters and settings

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

parameters <- defaultParameters()










## Ray light new style
##rayL <- rayLight(O=c(0,280,0))  ## returns a list of rays, can be plotted
##fullScene[['rl']] <- rayL ## Add to scene graph for plotting
##rayLFollow <- rayL %>% spectralize() %>% sendLight(univ,follow)
##fullScene[['rLF']] <- rayLFollow$scg


## Arc light new style
 ## arcL <- arcLight(steps=25)  ## returns a list of rays, can be plotted
 ## fullScene[['al']]  <- arcL ## Add to scene graph for plotting
 ## arcLFollow <- arcL %>% spectralize() %>% sendLight(univ,follow)
 ## fullScene[['aLF']] <- arcLFollow$scg




