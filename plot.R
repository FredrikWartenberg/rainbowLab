prepareData <- function(pd)
{
    pd$angInDeg <- as.factor(round(pd$angIn*180/pi))
    return(pd)
}

plot1 <- function(pd)
{
    p <- (
        ggplot(data=pd)
        + geom_point(
              aes(x=lambda,y=180+(angOut-angIn)*180/pi ,shape = angInDeg,color=color),size=5,show.legend=FALSE
          )
        + labs(title="Deflection Angles over Incident Angle and Lambda")
        ##+ theme(legend.title=element_blank())

    )

    print(p)

}
