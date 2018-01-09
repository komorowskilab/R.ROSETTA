\name{getDecision}
\alias{getDecision}
\title{
Reclassification.
}
\description{
Use rules to reclassify with old or new data.
}
\usage{
getDecision(df, rls, discretized=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame of feature values. Should contain the same feature names as the rules were made of.
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
library(RROSETTA)

df=autism
out=rosetta(df)
rls=out$main

decisions=getDecision(df, rls)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
