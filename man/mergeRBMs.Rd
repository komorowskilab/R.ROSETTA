\name{mergeRBMs}
\alias{mergeRBMs}
\title{
Merge rule-based models
}
\description{
Combines rule-based models into one model. 
}
\usage{
mergeRBMs(rbms, defClass=autcon$decision, fun="mean", pAdjust = TRUE, pAdjustMethod = "bonferroni")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rbms}{
A list of rule-based models in a strucutre of the main rosetta output.
}
  \item{defClass}{
A character vector containing the outcome values.
}
  \item{fun}{
A character. Define the function of merging rule statistic. Choose between "mean" or "sum". The default is "mean".
}
  \item{pAdjust}{
Logical. Set TRUE to apply rule p-value and relative risk p-value adjustment. Default is TRUE.
}
  \item{pAdjustMethod}{
A character containing the name of the method: holm, hochberg, hommel, bonferroni, BH, BY, fdr or none. Default is bonferroni.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{data frame}{Data frame of combined rules and their values.}
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

out1 <- rosetta(autcon, seed=1)
rbm1 <- out1$main

out2 <- rosetta(autcon, seed=2)
rbm2 <- out2$main

out3 <- rosetta(autcon, seed=3)
rbm3 <- out3$main

rbms <- list(rbm1, rbm2, rbm3)

#combine models
nrls <- mergeRBMs(rbms, fun = "mean", defClass = autcon$decision)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
