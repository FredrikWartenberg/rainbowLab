library(rgl)
library(data.table)
library(pracma)

source("generics.R")
source("render.R")
source("drop.R")
source("ray.R")
source("arrow.R")
source("path.R")
source("physics.R")

## constants and parameters
rbP <- list("box" = 1000, "dropR" = 400)

## display the window
r3dDefaults ## reset
open3d(windowRect = c(100,100,1000,1000))
bg3d(color=c("#664455"))

## Build the static scene
fullScene <- sceneGraph()
rDrop <- drop(x=500,R=rbP$dropR, color="blue")
fullScene[['rDrop']] <- rDrop


## add coordinate system
coordSys <- sceneGraph()
coordSys[['x']] <- arrow(p0=c(-rbP$box,0,0),p1=c(rbP$box,0,0),s=1/20,color="red")
coordSys[['y']] <- arrow(p0=c(0,-rbP$box,0),p1=c(0,rbP$box,0),s=1/20,color="green")
coordSys[['z']] <- arrow(p0=c(0,0,-rbP$box),p1=c(0,0,rbP$box),s=1/20,color="black")
fullScene[['coordSys']] <- coordSys



## ## test ray intersection
#r1 <- ray(O=c(-200,150,0),lambda=600)
##s1 <- launch(r1,rDrop,maxInteractions=3)
##s1$scg[['r1Exit']] <- fanOut(s1$ray,10000)
##fullScene[['r1']] <- s1$scg
nI = 4
r1 <- ray(O=c(-200,150,0),lambda=450)
fullScene[['r1']] <- follow(r1,rDrop,t=20000,nInt = nI)

r2 <- ray(O=c(-200,150,0),lambda=550)
fullScene[['r2']] <- follow(r2,rDrop,t=20000,nInt = nI)

r3 <- ray(O=c(-200,150,0),lambda=650)
fullScene[['r3']] <- follow(r3,rDrop,t=20000,nInt = nI)

r4 <- ray(O=c(-200,150,0),lambda=700)
fullScene[['r4']] <- follow(r4,rDrop,t=20000,nInt = nI)


## s2 <- launch(r2,rDrop,maxInteractions=3)
## fullScene[['r2']] <- s2$scg

## r3 <- ray(O=c(-200,150,0),lambda=700)
## s3 <- launch(r3,rDrop,maxInteractions=3)
## fullScene[['r3']] <- s3$scg


## renderstatic  scene
render(fullScene)




