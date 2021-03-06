% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateRainbows.R
\name{calculateIntensity}
\alias{calculateIntensity}
\title{Intensity change}
\usage{
calculateIntensity(lambda, thetaI, thetaE, thetaR, nR, method = "identity")
}
\arguments{
\item{lambda}{Wavelength nm}

\item{thetaI}{Incident angle of ray into drop}

\item{thetaE}{excident angle of ray from drop}

\item{thetaR}{reflection angle inside drop}

\item{nR}{number of reflections}

\item{method}{intensity calculation method (see details=}
}
\value{
attenuation value in linear domain (0..1)
}
\description{
Calculate change in intensity of ray propagating through the water drop
}
\details{
The identity method set attentuation to 1 (no attenuation)
The fresnel method calculates attenuation based on reflection
and transmission coefficients.
}
\author{
Fredrik Wartenberg
}
