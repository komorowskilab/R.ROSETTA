\name{randRules}
\alias{randRules}
\title{
Rule generator
}
\description{
Creates a predefined set of rules.
}
\usage{
randRules(nFeatures=150, nObjects=100, nOutcome=2, nRules=500, maxRuleLength=3, nDiscStates=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nFeatures}{
A numeric value of features number. The default is 150.
}
  \item{nObjects}{
A numeric value of objects number. The default is 100.
}
  \item{nOutcome}{
A numeric value of number of decision classes. The default is 2.
}
  \item{nRules}{
A numeric value of total number of rules. The default is 500.
}
  \item{maxRuleLength}{
A numeric value of maximum rule length. The default is 3.
}
  \item{nDiscStates}{
A numeric value of discretization states of the features. The default is 3.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{ruleTab}{A data frame of randomly generated rules split into 4 columns. Sorted by accuracy.}
  \item{rulesIT}{A character vector of randomly generated rules in IF-THEN format.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mateusz Garbulowski
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library(R.ROSETTA)

rules=randRules()
#save rules to line by line format
write.table(rules$ruleTab, "randomRulesLBL.txt", col.names = F, row.names = F, quote = F, sep="\t")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
