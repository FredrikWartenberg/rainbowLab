## Libraries and includes
library(rgl)
library(data.table)
library(pracma)
library(magrittr)

source("drop.R")
source("ray.R")
source("shapes.R")
source("path.R")
source("physics.R")
source("maths.R")

## Define display window
clear3d(type="all")
open3d(windowRect = c(100,100,1000,1000))
bg3d(color=c("#664455"))

## Build the static background scene
fullScene <- sceneGraph()
##fullScene[['coordSys']] <- coordSys() # add coordinate system

## universe of interacting objects
univ <- universe()
univ[['d1']] <- drop(x=401,R=400, color="blue")
fullScene[['univ']] <- univ ## add universe to scene

## Ray light, old style
##r1 <- ray(O=c(-200,150,0),lambda=450)
##rL <- rayLight(r1,spectrum=uniformSpectrum(steps=10))
##fullScene[['rl']] <- rL
##sL <- sendLight(rL$rays,univ,observer=follow)
##fullScene[['sL']] <- sL$scg

## Arc light old style
##al <- arcLight(steps=10)
##fullScene[['al']] <- al
##sL <- sendLight(al$rays,univ,observer=follow)
##fullScene[['sL']] <- sL$scg

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

## Arc light new style
lineL <- lineLight(steps=8,end=c(-100,370,0))  ## returns a list of rays, can be plotted
fullScene[['al']]  <- lineL ## Add to scene graph for plotting
lineLFollow <- lineL %>% spectralize() %>% sendLight(univ,follow)
fullScene[['lLF']] <- lineLFollow$scg


## returns the longer list of rays, plus the exit list, plus the datatable for evaluation
## data structure



##sal <- arcLight() %>% spectralize()   ## returns a list of rays, can be plotted
## <- rayLight()
##ullScene[['al']] <- arcLight()

## %>% sendLightNew(univ) ## returns the longer list of rays, plus the exit list, plus the datatable for evaluation
## %>% plot()

##render(...)


## render
render(fullScene)



