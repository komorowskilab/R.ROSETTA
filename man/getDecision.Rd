\name{getDecision}
\alias{getDecision}
getDecision
\title{
Retrieving decision
}
\description{
Retrieving decisions from data frame without decision column.
}
\usage{
getDecision(df, rls, discretized=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Input decision table. Without decision column.
}
  \item{rls}{
Output rules set from rosetta.
}
  \item{discretized}{
Set TRUE if your data are discretized. Default FALSE.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
library(rROSETTA)

df=autism
out=rosetta(df)
rls=out$main

decisions=getDecision(df, rls)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
