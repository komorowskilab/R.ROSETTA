\name{recalculateRules}
\alias{recalculateRules}
\title{
Recalcule rules
}
\description{
Recalcules rules according to the input decision table. Retrieves the true support sets for rules and calculates new p-values.
}
\usage{
recalculateRules(df, rls, discretized=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame containing decision table. The last column should contain decisions.
}
  \item{rls}{
A data frame of rules in a form like an output from rosetta.
}
  \item{discretized}{
Logical. If TRUE data are discrete. If FALSE data are not discrete values. Default FALSE.
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

df=autcon
out=rosetta(df)
rls=out$main

newRls=recalculateRules(df, rls)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
