\name{getFeatures}
\alias{getFeatures}
\title{
Retrieve important features.
}
\description{
Checks which features are the most significant and most frequent in rules per each class.
}
\usage{
getFeatures(df, rls, pval=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame of decision table. The last column should contain decisions.
}
  \item{rls}{
A data frame of rules in a form like an output from rosetta.
}
  \item{pval}{
A numeric value containing p-value cut. Default is set to 0.05 what means that we considering only significant rules.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{list}{A list of features per each class. Each list is sorted be importance.}
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
rls <- out$main

# default
features <- getFeatures(autcon, rls)

# change p-value
features <- getFeatures(autcon, rls, pval=0.01)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
