\name{recalculateRules}
\alias{recalculateRules}
\title{
Recalculate rules
}
\description{
Recalculate rules according to the input decision table. Retrieves the true support sets for rules and renew the p-values.
}
\usage{
recalculateRules(df, rls, discrete=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame containing decision table. The last column is decision.
}
  \item{rls}{
A data frame of rules in a form of rosetta output from $main.
}
  \item{discrete}{
Logical. Set TRUE if data are discrete. Default is FALSE.
}
    \item{pAdjust}{
Logical. If TRUE, p-value adjustment is applied. Default is TRUE.
}
  \item{pAdjustMethod}{
A character containing the name of the method: holm, hochberg, hommel, bonferroni, BH, BY, fdr or none.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{output}{A data frame containing recaluclated rules with: support, accuracy and lists of objects that support each rule.
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
