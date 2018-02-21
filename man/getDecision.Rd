\name{getDecision}
\alias{getDecision}
\title{
Reclassification.
}
\description{
Use rules to reclassify and validate with current or external data.
}
\usage{
getDecision(df, rls, discretized=F, normalize=T, normalizeMethod="scalar", validate=F, valiDec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame of feature values. Should contain the same feature names as the rules were made of.
}
  \item{rls}{
A data frame of rules in a form like an output from rosetta.
}
  \item{discretized}{
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
library(RROSETTA)

df=autcon
out=rosetta(df)
rls=out$main

#default
decisions=getDecision(df, rls)

#change normalization method
method="median"
decisions=getDecision(df, rls, normalize = T, normalizeMethod = method)

#with validation
oldDecisions=as.character(autism[,length(autism)])
decisions=getDecision(df, rls, validate = T, valiDec=oldDecisions)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
