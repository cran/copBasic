\name{ReineckeWell266}
\docType{data}
\alias{ReineckeWell266}
\title{Porosity and Permeability Data for Well-266 of the Reinecke Oil Field, Horseshoe Atoll, Texas}
\description{
These data represent porosity and permeability data from laboratory analysis for Well-266 Reinecke Oil Field, Horseshoe Atoll, Texas as used for the outstanding article by Saller and Dickson (2011). Dr. Art Saller shared a CSV file with the author of the \pkg{copBasic} package sometime in 2011. These data are included in this package because of the instruction potential of the bivariate relation between permeability and porosity.
}
\usage{
data(ReineckeWell266)
}
\format{
An \R \code{data.frame} with
\describe{
  \item{WELLNO}{The number of the well, no. 266;}
  \item{DEPTH}{The depth in feet to the center of the incremental spacings of the data;}
  \item{FRACDOLOMITE}{The fraction of the core sample that is dolomite, 0 is 100 percent limestone;}
  \item{Kmax}{The maximum permeability without respect to orientation in millidarcies;}
  \item{POROSITY}{The porosity of the core sample; and}
  \item{DOLOMITE}{Is the interval treated as dolomite (1) or limestone (0).}
}
}
\references{
Saller, A.H., Dickson, J.A., 2011, Partial dolomitization of a Pennsylvanian limestone buildup by hydrothermal fluids and its effect on reservoir quality and performance: AAPG Bulletin, v. 95, no. 10, pp. 1745--1762.
}
\examples{
\dontrun{
data(ReineckeWell266)
summary(ReineckeWell266) # show summary statistics}
}
\keyword{datasets}
