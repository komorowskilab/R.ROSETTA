\name{synData}
\alias{synData}
\title{
Decision table generator
}
\description{
Creates a decision table of correlated features.
}
\usage{
synData <- function(nFeatures=c(10,5,3,2,2), R=c(0.01,0.05,0.3,0.5,0.65), nObjects=120, nOutcome=2, unbalanced=F, pUnbalancedClass=0.8, seed=1){
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nFeatures}{
A numeric vector of features proportions. The default is c(10,5,3,2,2).
}
    \item{nObjects}{
A numeric vector of correlations. The default is c(0.01,0.05,0.3,0.5,0.65).
}
  \item{nObjects}{
A numeric value of objects number. The default is 120.
}
  \item{nOutcome}{
A numeric value of number of decision classes. The default is 2.
}
  \item{unbalanced}{
Logical. Set TRUE to generate unbalanced data. Default is FALSE.
}
  \item{pUnbalancedClass}{
A numeric value of number of unbalanced proportion for the first class. The default is 0.8.
}
\item{pUnbalancedClass}{
A numeric value of seed. The default is 1.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{output}{A data frame containing a decision table.}
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
df=synData()
out=rosetta(df)
out$quality
  
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
