\encoding{utf8}
\name{ReineckeWells}
\docType{data}
\alias{ReineckeWells}
\title{Porosity and Permeability Data for the Reinecke Oil Field, Horseshoe Atoll, Texas}
\description{
These data represent porosity and permeability data from laboratory analysis for the Reinecke Oil Field, Horseshoe Atoll, Texas as used for the outstanding article by Saller and Dickson (2011). Dr. A.H. Art Saller shared a CSV file with the author of the \pkg{copBasic} package sometime in 2011.  These data are included in this package because of the instruction potential of the bivariate relation between the geologic properties of permeability and porosity.
}
\usage{
data(ReineckeWells)
}
\format{
An \R \code{data.frame} with
\describe{
  \item{DOLOMITE}{The fraction of the core sample that is dolomite, 0 is 100 percent limestone;}
  \item{Kmax}{The maximum permeability without respect to orientation in millidarcies;}
  \item{K90}{The horizontal (with respect to 90 degrees of the borehole) permeability in millidarcies;}
  \item{Kvert}{The vertical permeability in millidarcies; and}
  \item{POROSITY}{The porosity of the core sample.}
}
}
\references{
Saller, A.H., Dickson, J.A., 2011, Partial dolomitization of a Pennsylvanian limestone buildup by hydrothermal fluids and its effect on reservoir quality and performance: AAPG Bulletin, v. 95, no. 10, pp. 1745--1762.
}
\examples{
\dontrun{
data(ReineckeWells)
summary(ReineckeWells) # show summary statistics}
}
\keyword{datasets}
