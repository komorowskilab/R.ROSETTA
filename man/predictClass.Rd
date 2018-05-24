\name{predictClass}
\alias{predictClass}
\title{
Class prediction.
}
\description{
Use model to predict classes.
}
\usage{
predictClass(df, rls, discrete=F, normalize=T, normalizeMethod="scalar", validate=F, valiDec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame of decision table. The last column is decision.
}
  \item{rls}{
A data frame of rules in a form of rosetta output from $main.
}
  \item{discrete}{
Logical. If TRUE data are discrete. If FALSE data are not discrete values. Default FALSE.
}

  \item{normalize}{
Logical. If TRUE votes are normalized. If FALSE normalization of votes is not performed. Default TRUE.
}

  \item{normalizeMethod}{
A character containing votes normalization method: median, mean, max, scalar, rulnum. Default is scalar.
}

  \item{validate}{
Logical. If TRUE decisions are validated using known ones. Default FALSE.
}

  \item{valiDec}{
A vector containing decisions to validate. Should be a character vector. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{decisions$out}{A data frame containing names of objects, votes for each decision and predicted decisions.
}
  \item{decisions$accuracy}{A numeric value containing an accuracy of the model.
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
rls <- out$main

# default
decisions <- predictClass(autcon, rls)

# change normalization method
decisions <- predictClass(autcon, rls, normalize = T, normalizeMethod = "median")

# with validation
oldDecisions <- as.character(autism[,length(autism)])
decisions <- predictClass(autcon, rls, validate = T, valiDec=oldDecisions)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
