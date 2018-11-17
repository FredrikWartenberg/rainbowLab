## Libraries and includes
library(rgl)
library(data.table)
library(pracma)

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
##univ[['d2']] <- drop(x=500,y=250,R=400, color="green")
fullScene[['univ']] <- univ ## add universe to scene

## Light sources
## r1 <- ray(O=c(-200,150,0),lambda=450)
##rl <- rayLight(r1,spectrum=uniformSpectrum(steps=10))
##fullScene[['rl']] <- rl

al <- arcLight(steps=200)
fullScene[['al']] <- al

## let light interact with scene and add to scene graph
sL <- sendLight(al$rays,univ,observer=follow)
fullScene[['sL']] <- sL$scg

## r2 <- ray(O=c(-200,250,-50),lambda=450)
## rs <- rayLight(r2,spectrum=uniformSpectrum(steps=10))
## fullScene[['s2']] <-  sendLight(rs,univ,observer=follow)


## render
render(fullScene)



