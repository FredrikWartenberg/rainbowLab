


pf2 <- function(n=1.33,by=0.1)
{

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
        + labs(title = paste("Fresenel Reflection and Transmission coeffcients for n =",
                             n, " (i) and air (o; n = 1)"),
               x = "Theta [deg]",
               y = "Coefficient")

        ## Design
        + theme_curves()

        ##+ scale_color_grey()
    )
    print(p)


}
