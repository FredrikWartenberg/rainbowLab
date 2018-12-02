## Experiment for single ray
## with debug inof
source("./scene.R")

## Turn on ray nromals and planes
parameters[['showNormals']] <- FALSE ## TRUE
parameters[['showRefractionPlane']] <- FALSE ##TRUE
parameters[['nInteractions']] <- 3
parameters[['outRayLength']] <- 200


## Define a single light ray
lightRay <- rayLight(O=c(-400,380,0),D=c(1,0,0))
## Add ray to scene for rendering
fullScene[['ll']]  <- lightRay

## apply spectrum to the ray
## for each wavelength one ray is generated
## parallel to the original ray
## here we use a monochromatic
## light source with lambda = 400 nm
spectrumRays <- spectralize(lightRay,spectrum=monochromaticSpectrum(c(400,600)))

## now send the ligth through the universe (= light drop)
tracedRays <- sendLight(spectrumRays,univ,follow,parameters)
## ... the results are in the structure tracedRays:
## tracedRayss$scg containts the scene graph for redering
## tracedRays$rayData contains the table with the angles

## add to scene for rendering
fullScene[['tracedRays']] <- tracedRays$scg

## render
renderScene(fullScene)
