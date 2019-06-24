\name{plotRule}
\alias{plotRule}
\title{
Plot values for a rule.
}
\description{
Represent rule as a heatmap or boxplot.
}
\usage{
plotRule(dt, rules, type="heatmap", discrete=FALSE, ind=15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
A data frame of decision table. The last column is decision.
}
  \item{rules}{
A data frame of rule table in a form of rosetta $main output.
}
  \item{discrete}{
Logical. Set TRUE for non-discrete data. Default is FALSE.
}
  \item{type}{
A character indciating plot type. Heatmap or boxplot.
}
  \item{ind}{
A numeric value of index of the rule in the table.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{heatmap}{A heatmap representing objects distribution in the model for the rule.}
}
\value{
  \item{boxplot}{A boxplots representing objects distribution in the model for the rule.}
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

plotRule(autcon, recRules, type="heatmap", discrete=FALSE, ind=3)
plotRule(autcon, recRules, type="boxplot", discrete=FALSE, ind=3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
