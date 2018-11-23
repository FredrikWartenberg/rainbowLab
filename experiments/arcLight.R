## Experiment for single ray
## with debug inof
source("./scene.R")

## Turn on ray nromals and planes
parameters[['showNormals']] <- FALSE
parameters[['showRefractionPlane']] <- TRUE
parameters[['nInteractions']] <- 3


## Define a single light ray
lightRays <- arcLight(fromAngle=0,toAngle=pi*0.4,steps=50)
## Add ray to scene for rendering
fullScene[['ll']]  <- lightRays

## apply spectrum to the ray
## for each wavelength one ray is generated
## parallel to the original ray
## here we use a monochromatic
## light source with lambda = 400 nm
spectrumRays <- spectralize(lightRays,spectrum=monochromaticSpectrum(400))


## now send the ligth through the universe (= light drop)
tracedRays <- sendLight(spectrumRays,univ,follow,parameters)
## ... the results are in the structure tracedRays:
## tracedRayss$scg containts the scene graph for redering
## tracedRays$rayData contains the table with the angles

## add to scene for rendering
fullScene[['tracedRays']] <- tracedRays$scg

## result to data
pd <- prepareData(tracedRays$rayData)
fwrite(file="experiments/arcLight.csv",pd)
## Plots

plot2(pd)

## render
render(fullScene)
