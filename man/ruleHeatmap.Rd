\name{ruleHeatmap}
\alias{getFeatures}
\title{
Plot heatmap of the rule.
}
\description{
Plotting heatmap of objects supporting given rule. The background is the rest of classes.
}
\usage{
getFeatures(df, rls, pval=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame of decision table. The last column should contain decisions.
}
  \item{rls}{
A data frame of recalculated rules.
}
  \item{ind}{
A numeric value of index of rule.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{list}{A heatmap presenting values supporting the rule.}
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
library(RROSETTA)
df=autism
out=rosetta(df)
rls=out$main
rls2=recalculateRules(df, rls)

ruleHeatmap(df, rls2, ind=6)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
