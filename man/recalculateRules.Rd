\name{recalculateRules}
\alias{recalculateRules}
\title{
Recalculate the rule table.
}
\description{
Recalculates the values in the rule table according to the input decision table. Retrieves the rule support sets.
}
\usage{
recalculateRules(dt, rules, discrete=FALSE, pAdjust=TRUE, pAdjustMethod="BH")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
A data frame of decision table. The last column is decision.
}
  \item{rules}{
A data frame of rule table from rosetta main output.
}
  \item{discrete}{
Logical. If TRUE the dataset is discrete. Default is FALSE. 
}
    \item{pAdjust}{
Logical. If TRUE, p-value adjustment is applied. Default is TRUE.
}
  \item{pAdjustMethod}{
A character containing the name of the method: holm, hochberg, hommel, bonferroni, BH, BY, fdr or none. Default is BH.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{output}{A data frame containing recaluclated rules with: feature names, accuracy, support, normalized support and support sets.
}
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

out <- rosetta(autcon)
rules <- out$main

recRules <- recalculateRules(autcon, rules)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
