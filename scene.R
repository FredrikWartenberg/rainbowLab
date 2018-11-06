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

## add the drop
drop <- drop(x=500,R=rbP$dropR, color="blue")
render(drop)

## add coordinate system
coordSys <- sceneGraph()
coordSys[['x']] <- arrow(p0=c(-rbP$box,0,0),p1=c(rbP$box,0,0),s=1/20,color="red")
coordSys[['y']] <- arrow(p0=c(0,-rbP$box,0),p1=c(0,rbP$box,0),s=1/20,color="green")
coordSys[['z']] <- arrow(p0=c(0,0,-rbP$box),p1=c(0,0,rbP$box),s=1/20,color="black")
render(coordSys)
##arrow3d(p0=c(-rbP$box,0,0),p1=c(rbP$box,0,0),s=1/20,color="red")
##arrow3d(p0=c(0,-rbP$box,0),p1=c(0,rbP$box,0),s=1/20,color="green")
##arrow3d(p0=c(0,0,-rbP$box),p1=c(0,0,rbP$box),s=1/20,color="black")



## test ray intersection
r1 <- ray(O=c(-200,-150,0))
r1$color <- "yellow"

## Intersect
i <- intersect(r1,drop)
render(r1,i$t2)

## calculate surface point and normal vector
sp <- point.ray(r1,i$t2) ## intersection point
nv <- normal.drop(drop,sp)
arrow3d(drop$O,sp,color="red")
arrow3d(sp,sp+nv*200,s=1/2,color="red")

## plot normal plane
##planes3d(nv,d= - nv %*% sp,  alpha=0.1)

## plot plane containing intersection point, ray and normal vector
nn1 <- cross(sp-drop$O,sp)
nn1 <- nn1 * c(1/norm(nn1,type="2"))
planes3d(nn1,d = - nn1 %*% sp, alpha=0.1)

## now generate second ray, which is refracted into the drop
theta = abs(c(acos(r1$D %*% nv)))
if(theta > pi/2)
{
    theta <- theta - pi/2
}

rm <- rotationMatrix(nn1,theta*2)
r2 <- ray(O=sp,D=r1$D %*% rm,color="green")
render(r2,t=300)

## test refraction o2i
o2i <- refract(r1,i$t2,drop,dir="o2i")
i2 <- intersect(o2i,drop)
render(o2i,t=i2$t1)

## test reflection
i2i <- reflect(o2i,i2$t1,drop)
i3 <- intersect(i2i,drop)
render(i2i,t=i3$t1)

## test refraction i2o
i2o <- refract(i2i,i2$t1,drop,dir="i2o")
i2o$color = "black"
render(i2o,t=600)
## plot normal vector
dp <- point.ray(i2i,i3$t1)
nv2 <- normal.drop(drop,dp)
arrow3d(dp,dp+nv2*200,s=1/2,color="green")


