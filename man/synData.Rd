\name{synData}
\alias{synData}
\title{
Decision table generator.
}
\description{
Creates a decision table of correlated features.
}
\usage{
synData(nFeatures=c(10,5,3,2,2), rf=c(0.2,0.2,0.2,0.2,0.2), rd=c(0.4,0.5,0.6,0.7,0.8),
nObjects=120, nOutcome=2, unbalanced=F, pUnbalancedClass=0.8, 
discrete=F, levels=4, labels=c("A","C","G","T"), seed=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nFeatures}{
A numeric vector of features proportions. The default is c(10,5,3,2,2).
}
  \item{rf}{
A numeric vector of correlations within feature set.
}
  \item{rd}{
A numeric vector of correlations between each feature and decision.
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
\item{seed}{
A numeric value of seed. The default is 1.
}
\item{discrete}{
Logical. Set TRUE to generate discrete data. Default is FALSE.
}
\item{levels}{
A numeric value of discretization levels. The default is 4.
}
\item{labels}{
A character vector of discretization labels for levels of discretization. 
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

### continuous data ###

## weak correlation
df1 <- synData(nFeatures=c(5,5,3,2,2), rf=c(0.2,0.3,0.2,0.4,0.4), rd=c(0.2,0.3,0.4,0.3,0.4))
out1 <- rosetta(df1)
out1$quality ## accuracy = 60%

## medium correlation
df2 <- synData(nFeatures=c(5,5,3,2,2), rf=c(0.2,0.3,0.2,0.4,0.4), rd=c(0.4,0.4,0.6,0.6,0.7))
out2 <- rosetta(df2)
out2$quality ## accuracy = 75%

## strong correlation
df3 <- synData(nFeatures=c(5,5,3,2,2), rf=c(0.2,0.3,0.2,0.4,0.4), rd=c(0.5,0.7,0.7,0.8,0.8))
out3 <- rosetta(df3)
out3$quality ## accuracy = 90%

### discrete data ###

dfd <- synData(nFeatures=c(5,5,3,2,2), rf=c(0.2,0.3,0.2,0.4,0.4), 
               rd=c(0.2,0.3,0.4,0.5,0.6), discrete = T, levels = 3, labels = c("low", "medium", "high"))
outd <- rosetta(dfd, discrete = T)
outd$quality ## accuracy = 85%

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
