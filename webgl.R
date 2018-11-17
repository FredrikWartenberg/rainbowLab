library(rgl)

save <- getOption("rgl.useNULL")
options(rgl.useNULL=TRUE)
##example("plot3d", "rgl")
render(fullScene)
widget <- rglwidget()
if (interactive())
  widget


# Save it to a file.  This requires pandoc
filename <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(rglwidget(), filename)
browseURL(filename)



