\name{R.ROSETTA-package}
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
Garbulowski M, Diamanti K, Smolińska K, Baltzer N, Stoll P, Bornelöv S, Øhrn A, Feuk L, Komorowski J. R.ROSETTA: an interpretable machine learning framework. BMC Bioinformatics. 2021 Mar 6;22(1):110. doi: 10.1186/s12859-021-04049-z
}
\keyword{package}
\seealso{
\link[R.ROSETTA article]{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-021-04049-z}
\link[ROSETTA]{https://bioinf.icm.uu.se/rosetta}
}
\examples{

  ## an example dataset is included in autcon variable
  resultsRos <- rosetta(autcon)
  
  ## print the quality of rule-based model
  print(resultsRos$quality)
  
  ## save the rule table for rules with P < 0.05
  rules <- resultsRos$main[which(resultsRos$main$pValue < 0.05),]
  
  ## print rules in IF-THEN form
  viewRules(rules, setDiscLabels = FALSE, newDiscLabels=c("down", "nochange",  "up"))

}
