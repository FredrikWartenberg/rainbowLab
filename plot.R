prepareData <- function(pd)
{
    pd$angInDeg <- round(pd$angIn*180/pi,3)
    pd$angOutDeg <- round(pd$angOut*180/pi,3)
    pd$angRefDeg <- round(pd$angRef*180/pi+180,3)
    pd$angD2Deg <- round(pd$angD*180/pi,3)
    pd$angD2DegCorr <- 180-pd$angD2Deg
    pd[ni > 4, angD2DegCorr := angD2Deg]
    pd$rainbowNo <- as.factor(pd$ni - 2)
    ##pd[angDiffDeg > 180,angDiffDeg:= angDiffDeg -180]
    return(pd)
}


plotInVsOut <- function(pd)
{
    p <- (
        ggplot(data=pd)
        + geom_point(aes(
              x     =angInDeg,
              y     = angD2DegCorr,
              color = as.factor(lambda),
              shape = rainbowNo),
              size=4)

        ## Scales
        + scale_color_manual(values=unique(pd$color))
        + scale_x_continuous(breaks = seq(from=0,to=180,by=10))
        + scale_y_continuous(breaks = seq(from=0,to=180,by=10))
        + scale_shape_manual(values = c(15,16,17,18))
        ## Legends
        + labs(title = "Difference between inicdent and emitting angle",
               subtitle = "vs. lambda and rainbow number",
               shape = "Rainbow No",
               color = "Lambda [nm]",
               x     = "Incident angle relative to surface normal [deg]",
               y     = "Difference between incident and emitting angle [deg]")

        ## Theme and design
        + theme_minimal()
    )
    print(p)
}

plotPDF <- function(pd)
{
    p <- (
        ggplot(data=pd[angD2DegCorr <60&angD2DegCorr >35],
               aes(angD2DegCorr))
        + geom_histogram(bins=20)
        + facet_grid(lambda~rainbowNo)


        ## Legends
        + labs(title = "Probability Distribution of Angular Difference",
               subtitle = "vs. lambda and rainbow number",
               x     = "Difference between incident and emitting angle [deg]")

    )
    print(p)
}
