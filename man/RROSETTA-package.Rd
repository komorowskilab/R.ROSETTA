\name{RROSETTA-package}
\alias{RROSETTA-package}
\alias{RROSETTA}
\docType{package}
\title{
\packageTitle{RROSETTA}
}
\description{
\packageDescription{RROSETTA}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{RROSETTA}
\packageIndices{RROSETTA}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{RROSETTA}

Maintainer: \packageMaintainer{RROSETTA}
}
\references{
A. Ohrn, J. Komorowski (1997), ROSETTA: A Rough Set Toolkit for Analysis of Data, Proc. Third International Joint Conference on Information Sciences, Fifth International Workshop on Rough Sets and Soft Computing (RSSC'97), Durham, NC, USA, March 1-5, Vol. 3, pp. 403-407.
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
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
