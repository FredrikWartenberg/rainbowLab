## calculate R, R_s and R_p by means of fresnel equations
## R is the average of R_s and R_p for use when polarisation
## is not considered
fresnel <- function(theta,n,dir)
{

    cosI <- cos(theta)
    ## calculate cos of other angle
    if(dir == "o2i")
    {
        cosE <- sqrt(1- (sin(theta)/n)^2)
        nI = 1
        nE = n
    } else if(dir == "i2o" | dir == "i2i")
    {
        cosE <- sqrt( 1- (sin(theta)*n)^2)
        nI = n
        nE = 1
    }

    ## common denominator
    den = nI * cosI + nE * cosE

    ## R's
    R_s = ((nI * cosI - nE * cosE) / den)^2
    R_p = ((nI * cosE - nE * cosI) / den)^2
    R   = 0.5 * (R_s + R_p)

    return(list('R' = R, 'R_p' = R_p, 'R_s' = R_s, 'T' = 1 - R , 'T_s' = 1 - R_s, 'T_p' = 1 - R_p))
}

## plot fresenl for reflection on a water surface
plotFresnel <- function(n=1.33,by=0.1)
{
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

        ## Annotation
        + labs(title = paste("Fresenel Reflection and Transmission coeffcients for n =", n, "and air (o)"),
               x = "Theta [deg]",
               y = "Coefficient")

        ## Design
        + theme_curves()
        ## + scale_colour_manual(values = c("red", "blue", "green"))
        + scale_color_grey()
    )
    print(p)
}
