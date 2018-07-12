\name{saveLineByLine}
\alias{saveLineByLine}
\title{
Converts rules to LineByLine format.
}
\description{
Saves rule table as LineByLine txt format that is compatible with VisuNet and Ciruvis framework.
}
\usage{
saveLineByLine(rules, path, discrete=FALSE, geneExprData=TRUE, filterByPval=FALSE, pval=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rules}{
A data frame of rule table in a form of rosetta output.
}
  \item{path}{
Path to save the output file. Should contain the name of the file with .txt extension.
}
  \item{discrete}{
Logical. If TRUE data are discrete. If FALSE data are not discrete values. Default FALSE.
}
  \item{geneExprData}{
Logical. If TRUE data are gene expression with 2 or 3 classes. Default is TRUE.
}
  \item{filterByPval}{
Logical. If TRUE the filtration is performed. Default is FALSE.
}
  \item{pval}{
A numeric value containing p-value threshold. Default is 0.05.
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
library(R.ROSETTA)

out=rosetta(autcon)
rules=out$main

# default
saveLineByLine(rules, "outFile.txt")

# filter by p-value
saveLineByLine(rules, "outFile.txt", filterByPval=TRUE, pval=0.01)

# for non discrete gene expression data 
saveLineByLine(rules, "outFile.txt", geneExprData=TRUE, discrete=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
