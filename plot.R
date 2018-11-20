prepareData <- function(pd)
{
    pd$angInDeg <- as.factor(round(pd$angIn*180/pi))
    pd$angOutDeg <- as.factor(round(pd$angOut*180/pi))
    pd$angDiffDeg <- as.factor(round((pd$angOut-pd$angIn)*180/pi))
    return(pd)
}

plot1 <- function(pd)
{
    p <- (
        ggplot(data=pd)
        + geom_point(
              aes(x=lambda,y=180+(angOut-angIn)*180/pi ,shape = angInDeg)
          )
        + labs(title="Deflection Angles over Incident Angle and Lambda")
        ##+ theme(legend.title=element_blank())

    )

    print(p)

}

plot2 <- function(pd)
{
    p <- (
        ggplot(data=pd)
        + geom_point(
              aes(x=lambda,y=180+(angOut)*180/pi ,color = color)
          )
        + facet_grid(.~angInDeg)
        + theme(legend.title=element_blank())
        + labs(title="Deflection Angles over Incident Angle and Lambda")
        + theme(legend.title=element_blank())

    )

    print(p)

}
