\name{saveLineByLine}
\alias{saveLineByLine}
\title{
Saving rules to line by line format.
}
\description{
Format line by line is compatible with VisuNet.
}
\usage{
saveLineByLine(rulesTab, path, geneExprData = TRUE, pval = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rulesTab}{
Output from rosetta function. Set of rules in data.frame.
}
  \item{path}{
Path to save the output file.
}
  \item{discretized}{
Set TRUE if your data are discretized. Default is FALSE.
}
  \item{geneExprData}{
Set TRUE if data are gene expression with 2 or 3 classes. Default is FALSE.
}
  \item{pval}{
Filter rules by pvalue. Default is TRUE.
}
  \item{pval}{
Set pvalue to filter out rules. Default is 0.05.
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
#### SAVE RESULTS IN LINE BY LINE #### FILTER OUT BY PVALUE
##general
saveLineByLine(resultsRos$main, "outFile.txt")

##change pvalue
saveLineByLine(resultsRos$main, "outFile.txt", pval=0.01)

##discretized gene expression data 
saveLineByLine(resultsRosGR$main, "outFile.txt", geneExprData=TRUE, discretized=TRUE)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
