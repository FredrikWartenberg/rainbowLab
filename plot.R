## Plots

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
        ggplot(data=pd,
               aes(angDDeg,
          ##         color=as.factor(rainbowNo),
                   fill =as.factor(lambda)
                   ),size=2)
        + geom_histogram(bins=200)
        ##+ geom_freqpoly(bins=200)
        ##+ facet_grid(lambda~.)

        ## scales
        + scale_fill_manual(values=unique(pd$color))
        + coord_flip()

        ## Legends
        + labs(title = "Probability Distribution of Angular Difference",
               subtitle = "vs. lambda",## and rainbow number",
               x     = "Difference between incident and emitting angle [deg]",
               fill = "wavelength")

        ## Theme and design
        + theme_minimal()

    )
    print(p)
}


plotPDFLines <- function(pd)
{
    p <- (
        ##ggplot(data=pd[angDDeg <60&angDDeg >30],
        ggplot(data=pd,
               aes(angDDeg,
                   ##linetype = as.factor(rainbowNo),
                   ##size = as.factor(rainbowNo),
                   color    = as.factor(lambda)
                   ))
        + geom_freqpoly(bins=200,size=2)
        + facet_grid(rainbowNo~.)

        ## scales
        + scale_color_manual(values=unique(pd$color))
        ##+ coord_flip()

        ## Legends
        + labs(title = "Probability Distribution of Angular Difference",
               subtitle = "vs. lambda",## and rainbow number",
               x     = "Difference between incident and emitting angle [deg]",
               fill = "wavelength")

        ## Theme and design
        + theme_minimal()

    )
    print(p)
}
