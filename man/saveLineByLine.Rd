\name{saveLineByLine}
\alias{saveLineByLine}
\title{
Converts rules to LineByLine format.
}
\description{
Saves rules in LineByLine format that is compatible with VisuNet software. Allows to filter out not-significant rules by setting the threshold of p-value.
}
\usage{
saveLineByLine(rls, path, discretized=FALSE, geneExprData=TRUE, filterByPval=TRUE, pval=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rls}{
A data frame containing output from rosetta function.
}
  \item{path}{
Path to save the output file. Should contain name of the file in .txt format.
}
  \item{discretized}{
Logical. If TRUE data are discrete. If FALSE data are not discrete values. Default FALSE.
}
  \item{geneExprData}{
Logical. If TRUE data are gene expression with 2 or 3 classes. Default is FALSE.
}
  \item{filterByPval}{
Logical. If TRUE the filtration will be done. Default is TRUE.
}
  \item{pval}{
A numeric value containing p-value threshold allows filtering. Default is 0.05.
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

# default
saveLineByLine(rls, "outFile.txt")

# filter by p-value
saveLineByLine(rls, "outFile.txt", pval=0.01)

# for discrete gene expression data 
saveLineByLine(resultsRosGR$main, "outFile.txt", geneExprData=TRUE, discretized=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
