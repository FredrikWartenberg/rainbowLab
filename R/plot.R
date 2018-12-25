## Themes
theme_curves <- function(){

    theme_classic()+
    theme(
        title = element_text(size = 15),
        ##axis.ticks = element_blank(),
        ##axis.text.y = element_blank(),
        strip.text   = element_text(size = 15),
        axis.text.x  = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.line.x  = element_line(color="grey80"),
        axis.line.y  = element_line(color="grey80")
        ##axis.line.y = element_blank(),
        ##legend.position="none",
    )
}


## Plots based on rainbow data
## generate data by running
## pd <- generateRainbows(...)

plotInVsOut <- function(rbd)
{

    p <- (
        ggplot(data=rbd)
        + geom_point(aes(
              x     =angInDeg,
              y     = angDDeg,
              color = as.factor(lambda),
              shape = rainbowNo),
              size=4)

        ## Scales
        + scale_color_manual(values=unique(rbd$color))
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
                   color    = as.factor(lambda)
                   ))
        + geom_freqpoly(bins=200,size=2)
        + facet_grid(rainbowNo~.)
        ##+ coord_polar(start=-pi/2,direction=-1)
        ##+ scale_x_continuous(limits = c(0,360))

        ## scales
        + scale_color_manual(values=unique(pd$color))

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



plotMaxima <- function(dm)
{
    mama <- maxima(dm)
    p <- (
        ggplot(data=mama) +
        geom_point(aes(x=lambda,y=angD,color=rainbowNo),size = 2) +
        scale_y_continuous(breaks=seq(from=30,to=180,length = 16)) +
        labs(title = "Angle of Maximum Ray Density",
             x = "lambda [nm]",
             y = "Angle [deg]",
             color = "Rainbow number")

    )

    print(p)
}


## #############################################
## Plot physics functions
##

##' plot fresnel for reflection on a water surface
##'
##' Plots reflection and transmission for s and p polarisations
##' assumes air water interface and n air = 1
##' @title Plot Fresenel Coefficients (tit)
##' @param n refractive index
##' @param by angular steps (deg) for plotting
##' @return nothing
##' @author Fredrik Wartenberg
##' @export
plotFresnel <- function(n=1.33,by=0.1)
{
    ## Generate plot data
    ## angles & types
    s <- data.table('theta' = seq(from=0,to=90,by=by)*pi/180,'polarisation' = "s", kind = "Reflection")
    p <- data.table('theta' = seq(from=0,to=90,by=by)*pi/180,'polarisation' = "p", kind = "Reflection")

    ##  Reflection
    s$coeff <- fresnel(s$theta,n=n,dir="o2i")$R_s
    s$dir <- "o2i"
    p$coeff <- fresnel(p$theta,n=n,dir="o2i")$R_p
    p$dir <- "o2i"

    s2 <- s
    s2$coeff <- fresnel(s2$theta,n=n,dir="i2o")$R_s
    s2$dir <- "i2o"

    p2 <- p
    p2$coeff <- fresnel(p2$theta,n=n,dir="i2o")$R_p
    p2$dir <- "i2o"

    ## join
    fR <- rbind(s,s2,p,p2)

    ## replace NaN with one (total reflection)
    fR <- fR[is.na(coeff), coeff := 1]

    ## transmission
    fT <- fR
    fT$kind <- "Transmission"
    fT$coeff <- 1 - fT$coeff

    ## join
    fData <- rbind(fR,fT)


    ## join & plot
    p <- (

        ## Data
        ggplot(data=fData)

        ## Plot
        + geom_line(aes(x=theta*180/pi,y=coeff,color=polarisation,linetype=kind),size = 1)
        + facet_grid(.~dir)

        ## Scales (
        + scale_colour_manual(values = c("red", "blue", "green"))
        + scale_x_continuous(breaks = seq(0,90,by=10))

        ## Annotation
        + labs(title = paste("Fresnel Reflection and Transmission coeffcients for n =",
                             n, " (i) and air (o; n = 1)"),
               x = "Theta [deg]",
               y = "Coefficient")

        ## Design
        + theme_curves()

        ##+ scale_color_grey()
    )
    print(p)

    return(fData)
}


