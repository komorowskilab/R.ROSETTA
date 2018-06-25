\name{getFeatures}
\alias{getFeatures}
\title{
Retrieves feature lists from the rules.
}
\description{
Inspects the most significant and frequent features from the rules. Creates seperate list of the features for each decision class.
}
\usage{
getFeatures(dt, rules, pval=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
A data frame of decision table. The last column is decision.
}
  \item{rules}{
A data frame of rule table in a form of rosetta output.
}
  \item{pval}{
A numeric value containing p-value threshold. The default significance level is 0.05.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{list}{A list of the features for each class. Each list is sorted be the importance (calculated from the feature frequency and p-value).}
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

# default
features <- getFeatures(autcon, rules)

# change p-value
features <- getFeatures(autcon, rules, pval=0.01)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
