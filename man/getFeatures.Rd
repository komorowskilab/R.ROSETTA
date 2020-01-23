\name{getFeatures}
\alias{getFeatures}
\title{
Features list extraction
}
\description{
Extracts the features from the rules. Sorts the features according to their frequency (percentage). Creates a list of the features, frequencies and discretization levels for each decision class.
}
\usage{
getFeatures(rules, filter = F, filterType = "support", thr = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rules}{
A data frame of rule table in a form of main rosetta output.
}
  \item{filter}{
Logical. Enable filtration or not. Default is F.
}
  \item{filterType}{
A character. Choose between support, accuracy or pvalue. The default is 10.
}
  \item{thr}{
A numeric value of rule threshold. The default is 10.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{list}{Lists of the features, frequencies and discretization levels for each class. Each list is sorted be the frequency.}
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

out <- rosetta(autcon)
rules <- out$main

# default settings
features <- getFeatures(rules)

# filter by p-value
features <- getFeatures(rules, filter = T, filterType = "pvalue", thr = 0.05)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
