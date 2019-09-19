\name{plotMeanROC}
\alias{plotMeanROC}
\title{
Plots a ROC curve.
}
\description{
The ROC curve is respresented as average values from CV.
}
\usage{
plotMeanROC(out)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{out}{
A list. The output from R.ROSETTA.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{output}{A plot.}
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

out <- rosetta(autcon, roc = TRUE, clroc = "autism")
plotMeanROC(out)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
