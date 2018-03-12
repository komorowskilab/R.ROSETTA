\name{R.ROSETTA-package}
\alias{R.ROSETTA-package}
\alias{R.ROSETTA}
\docType{package}
\title{
\packageTitle{R.ROSETTA}
}
\description{
\packageDescription{R.ROSETTA}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{R.ROSETTA}
\packageIndices{R.ROSETTA}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{R.ROSETTA}

Maintainer: \packageMaintainer{R.ROSETTA}
}
\references{
A. Ohrn, J. Komorowski (1997), ROSETTA: A Rough Set Toolkit for Analysis of Data, Proc. Third International Joint Conference on Information Sciences, Fifth International Workshop on Rough Sets and Soft Computing (RSSC'97), Durham, NC, USA, March 1-5, Vol. 3, pp. 403-407.
}
\keyword{package}
\seealso{
\link[RROSETTA]{bioinf.icm.uu.se/rosetta}
}
\examples{
df=autism

resultsRos=rosetta(df)
resultsRos$statistic
resultsRos$rules

saveLineByLine(resultsRos$rules,"out.txt")
}
