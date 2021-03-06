% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physics.R
\name{fresnel}
\alias{fresnel}
\title{Fresnel Reflection and Transmission coefficients}
\usage{
fresnel(theta, n, dir)
}
\arguments{
\item{theta}{incident angle}

\item{n}{refractive index}

\item{dir}{o2i :outside (air) to inside (medium) or i2o :inside (medium) to outside (air)}
}
\value{
a list with reflection (R) and transmission (T) coefficients for s (perpendicular) and p (parallel) polarization and their mean.
Access like res$R_s etc; syntax {T,R}_{s,p,m}
}
\description{
calculate R, R_s and R_p by means of fresnel equations for
air/medium interface
}
\details{
R: Reflection coefficient
T: Transmission coefficient (1-R)
s: perpendicular polarizatio
p: parallel polarization
dir: direction
     o2i from air into medium
     i2o from medium into air
     i2i from medium to medium (same as i2o)
R is the average of R_s and R_p for use when polarisation  is not considered
Source: https://de.wikipedia.org/wiki/Fresnelsche_Formeln
}
\author{
Fredrik Wartenberg
}
