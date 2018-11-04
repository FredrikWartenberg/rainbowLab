library(rgl)
library(data.table)
library(pracma)

source("generics.R")
source("drop.R")
source("ray.R")

## constants and parameters
rbP <- list("box" = 1000, "dropR" = 200)

## display the window
r3dDefaults ## reset
open3d(windowRect = c(100,100,1000,1000))
bg3d(color=c("#333333"))

## add the drop
drop <- drop(x=500,R=rbP$dropR, color="blue")
render(drop)

## add coordinate system
arrow3d(p0=c(-rbP$box,0,0),p1=c(rbP$box,0,0),s=1/20,color="black")
arrow3d(p0=c(0,-rbP$box,0),p1=c(0,rbP$box,0),s=1/20,color="blue")
arrow3d(p0=c(0,0,-rbP$box),p1=c(0,0,rbP$box),s=1/20,color="red")


## test ray intersection
r1 <- ray(x=-200)
r1$color <- "red"

## Intersect
i <- intersect(r1,drop)
render(r1,i$t1)

## calculate surface point and normal vector
sp <- point.ray(r1,i$t1)
nv <- normal.drop(drop,sp)
arrow3d(drop$O,sp,color="red")
arrow3d(sp,sp+nv*50,color="red")

## plot normal plane
planes3d(nv,d= - nv %*% sp,  alpha=0.1)

## plot plane containing intersection point, ray and normal vector
nn1 <- cross(sp-drop$O,sp)
nn1 <- nn1 * c(1/norm(nn1,type="2"))
planes3d(nn1,d = - nn1 %*% sp, alpha=0.1)

