\name{randDecisionTable}
\alias{randDecisionTable}
\title{
Decision table generator
}
\description{
Creates a decision table of random features.
}
\usage{
randDecisionTable(nFeatures=180, nObjects=120, nOutcome=2, discrete=F, nDiscStates=3, containStrings=F, nStrings=10, nClassStrings=3, unbalanced=F, pUnbalancedClass=0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nFeatures}{
A numeric value of features number. The default is 180.
}
  \item{nObjects}{
A numeric value of objects number. The default is 120.
}
  \item{nOutcome}{
A numeric value of number of decision classes. The default is 2.
}
  \item{discrete}{
Logical. Set TRUE to generate discrete data. Default is FALSE.
}
  \item{nDiscStates}{
A numeric value of number of discrete states. The default is 3.
}
  \item{containStrings}{
Logical. Set TRUE to add features of string type. Default is FALSE.
}
  \item{nStrings}{
A numeric value of number of features of string type. The default is 10.
}
  \item{nDiscStates}{
A numeric value of number of string features discrete states. The default is 3.
}
  \item{unbalanced}{
Logical. Set TRUE to generate unbalanced data. Default is FALSE.
}
  \item{pUnbalancedClass}{
A numeric value of number of unbalanced proportion for the first class. The default is 0.8.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{output}{A data frame containing random decision table.
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

#default settings
dt1=randDecisionTable()
out1=rosetta(dt1)
out1$quality

#decision with 3 classes
dt2=randDecisionTable(nOutcome=3)
out2=rosetta(dt2)
out2$quality

#discrete data with 2 states
dt3=randDecisionTable(discrete = T, nDiscStates=2)
out3=rosetta(dt3, discrete = T)
out3$quality

#unbalanced data without/with undersampling
dt4=randDecisionTable(unbalanced=T, pUnbalancedClass=0.8)
out4_1=rosetta(dt4)
out4_1$quality

out4_2=rosetta(dt4, underSample = T, underSampleNum = 5)
out4_2$quality


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
