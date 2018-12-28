## Themes
##' Theme for rainbow Lab
##'
##' Based on theme_classic
##' @title Rainbow Lab Plot theme
##' @return theme
##' @author Fredrik Wartenberg
theme_rainbowLab <- function(){

    theme_classic()+
    theme(
        title = element_text(size = 15),
        strip.text   = element_text(size = 13),
        axis.text.x  = element_text(size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.line.x  = element_line(color="grey80"),
        axis.line.y  = element_line(color="grey80")
    )
}


##' Plot angular difference vs incident angle
##'
##' Mostly a plot to visualize the data. Use plotPDF to
##' view rainbow angles and distributions.
##' @title Plot Incident vs. angular difference
##' @param rayData data.table with rayData
##' @return nothing
##' @author Fredrik Wartenberg
##' @export
plotInVsOut <- function(rayData)
{

    p <- (
        ggplot(data=rayData)
        + geom_point(aes(
              x     =angInDeg,
              y     = angDDeg,
              color = as.factor(lambda),
              shape = rainbowNo),
              size=4)

        ## Scales
        + scale_color_manual(values=unique(rayData$color))
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
        + theme_rainbowLab()
    )
    print(p)
}


##' Plot probablity density function for angular difference
##'
##' Plots the aggregated data in pdfData, no calculation is done here.
##' For different aggregation options see aggregateData
##' @title Plot angular difference PDF
##' @param pdfData data.table with aggregated rayData
##' @return nothing
##' @author Fredrik Wartenberg
##' @export
plotPDF <- function(pdfData)
{
    rbcols <- sapply(X=unique(pdfData$lambda),FUN=lambda2rgb)
    p <- (
        ggplot(data=pdfData, aes(x=angD, y = I, color = as.factor(lambda)))
        + geom_line(size = 2)
        + facet_grid(rainbowNo ~ . ,labeller = label_both)

        ## scales
        ##+ scale_color_manual(values=unique(rayData$color))
        + scale_color_manual(values= rbcols)

        ## Legends
        + labs(title    = "Probability Distribution of Angular Difference vs. lambda and rainbow number",
               subtitle = paste("Intensity Method =", attr(pdfData,"intensityMethod")),
               x        = "Difference between incident and emitting angle [deg]",
               y        = "probability [0..1]",
               color    = "Lambda [nm]")

        ## Theme and design
        + theme_rainbowLab()

    )
    print(p)
}

##' Plot Angles of Maximum Intensities for Rainbow x Lambda
##'
##' Use function maxima to calculate data points. See
##' maxima function for details.
##' @title Plot Angles of Maximum Intensities
##' @param pdfData data.table with aggregated rayData
##' @return data.table with maxima
##' @author Fredrik Wartenberg
##' @export
plotMaxima <- function(pdfData)
{
    ## Calculate data points
    mama <- maxima(pdfData)

    ## Plot
    p <- (

        ## Define Plot
        ggplot(data=mama)
        + geom_point(aes(x=lambda,y=angD,color=rainbowNo),size = 2)
        + geom_line(aes(x=lambda,y=angD,color=rainbowNo),size = 1)

        ## scales
        + scale_y_continuous(breaks=seq(from=30,to=180,length = 16))

        ## Anntotation
        + labs(title = "Angle of Maximum Ray Density",
             subtitle = paste("intensity method:",
                              attr(pdfData,"intensityMethod")),
             x = "lambda [nm]",
             y = "Angle [deg]",
             color = "Rainbow Number")

        ## Theme and design
        + theme_rainbowLab()

    )
    print(p)

    invisible(mama)
}

##' Plot Refractive Index of water over Lambda
##'
##' Prints a color coded plot of the refractive index over lambda
##' Data source: Segelstein 1981; The Complex Refractive Index of Water"
##' @title Plot Refractive Index
##' @return nothing
##' @author Fredrik Wartenberg
##' @export
plotRefractiveIndex <- function()
{
    ## generate data table
    lambda = seq(from   = physicalConstants()$visibleMin,
                 to     = physicalConstants()$visibleMax,
                 length = 100)
    data <- data.table(lambda)
    data$refractiveIndex <- refractiveIndex(data$lambda)
    col <- sapply(FUN=lambda2rgb,X=data$lambda)
    data$color <- col

    ## compose plot
    p <-
        (
            ## Define plot
            ggplot(data=data)

            + geom_point(aes(x=lambda,
                             y=refractiveIndex,
                             color=color),
                         show.legend=FALSE)

            ## Anntotation
            + labs(title = "Refractive Index for Water for Visible Light",
                   subtitle = "Source: Segelstein 1981; The Complex Refractive Index of Water",
                   x = "lambda [nm]",
                   y = "Refractive Index")

            ## Theme and design
            + theme_rainbowLab()
        )

    ## display
    print(p)

}


##' plot fresnel coefficients for water/air interface
##'
##' Plots reflection and transmission coeffcients for s and p polarisations
##' assumes air water interface and n air = 1
##' @title Plot Fresenel Coefficients
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
        + geom_line(aes(x=theta*180/pi,y=coeff,color=polarisation,linetype=kind),size = 2)
        + facet_grid(.~dir)

        ## Scales (
        + scale_colour_manual(values = c("#a1d99b", "#1f78b4", "green"))
        + scale_x_continuous(breaks = seq(0,90,by=10))

        ## Annotation
        + labs(title = paste("Fresnel Reflection and Transmission coeffcients for n =",
                             n, " (i) and air (o; n = 1)"),
               x = "Theta [deg]",
               y = "Coefficient")

        ## Design
        + theme_rainbowLab()

        ##+ scale_color_grey()
    )
    print(p)

    return(fData)
}

##' Plot relative intensities and width of rainbows
##'
##' Uses intensities calculated by fresnel method.
##' This is an unverified method. It is
##' difficult to find actual data on the intensities.
##' Stated relative intensities between first and second
##' rainbow range from 50% to 10%.
##' 3dB width is calculated as the difference between the
##' maximum and the minimum angle where the pdf aggregated
##' over all lambda is above 50% of the maxiumum value.
##' (see https://en.wikipedia.org/wiki/Beamwidth)
##' @title Plot relative rainbow intensities and 3dB width.
##' @param pdfData data.table with aggregated rayData
##' @return nothing
##' @author Fredrik Wartenberg
##' @export
plotIntensities <- function(pdfData)
{
    ## Calculate intensities
    its <- intensities(pdfData)

    ## Plot
    p <- (

        ## Plot definition
        ggplot(its)
        + geom_col(aes(x=rainbowNo,y=relIntensity),
                   width = its$width3db*pi/180,fill="#1f78b4")

        ## Text labels
        + geom_label(aes(x=as.numeric(rainbowNo)+0.2 ,y=relIntensity,label = paste(round(its$width3db,1),"deg")))


        ## Annotation
        + labs(title    = "Relative intensities and 3dB width of rainbows",
               subtitle = paste("Intensity Method:", attr(pdfData,"intensityMethod")),
               x        = "Rainbow No",
               y        = "Relative Intensity [%]",
               width    = "3dB width")

        ## Design
        + theme_rainbowLab()

    )
    print(p)
}
