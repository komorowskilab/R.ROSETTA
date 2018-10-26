\name{predictClass}
\alias{predictClass}
\title{
Predict new decision classes.
}
\description{
Use generated model to validate independent dataset.
}
\usage{
predictClass(dt, rules, discrete=FALSE, normalize=TRUE, normalizeMethod="rss", validate=FALSE, valiDec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
A data frame of decision table. The last column is decision.
}
  \item{rules}{
A data frame of rule table in a form of rosetta $main output.
}
  \item{discrete}{
Logical. If TRUE the dataset is discrete. Default is FALSE. 
}

  \item{normalize}{
Logical. If TRUE votes are normalized. If FALSE normalization of votes is not performed. Default is TRUE.
}

  \item{normalizeMethod}{
A character containing votes normalization method: median, mean, max, rss (root sum square), rulnum (rule number). Default is rss.
}

  \item{validate}{
Logical. If TRUE, new decisions are validated using fixed ones. Default is FALSE.
}

  \item{valiDec}{
A character vector containing decisions to validate.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{decisions$out}{A data frame containing names of the objects, votes for each decision and predicted outcome.
}
  \item{decisions$accuracy}{A numeric value containing an accuracy of the model. Only if validate is TRUE.
}

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
decisions <- predictClass(autcon, rules)

# change normalization method
decisions <- predictClass(autcon, rules, normalize = TRUE, normalizeMethod = "median")

# with validation
oldDecisions <- as.character(autism[,length(autism)])
decisions <- predictClass(autcon, rules, validate = TRUE, valiDec=oldDecisions)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
