\name{ruleHeatmap}
\alias{ruleHeatmap}
\title{
Plot heatmap of a rule.
}
\description{
Plotting a heatmap of objects supporting given rule. The background is the rest of classes.
}
\usage{
ruleHeatmap(df, rls, ind=1, nbins=3, showClust=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame of decision table. The last column should contain decisions.
}
  \item{rls}{
A data frame of rules.
}
  \item{ind}{
A numeric value of index of rule.
}
  \item{nbins}{
A numeric value of discretization bins.
}
  \item{showClust}{
Logical. Set TRUE to show clusters inside of each group within heatmap. Default is FALSE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{heatmap}{A heatmap presenting objects supporting the rule.}
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

out=rosetta(autcon)
rules=out$main
recRules=recalculateRules(autcon, rls)

ruleHeatmap(autcon, recRules, ind=1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
