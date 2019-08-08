\name{rosetta}
\alias{rosetta}
\title{
ROSETTA classifier
}
\description{
Creates a rule-based classification model.
}
\usage{
rosetta(dt, classifier="StandardVoter", cvNum=10, discrete=FALSE, discreteMethod="EqualFrequency",
discreteParam=3, discreteMask=TRUE, reducer="Johnson", reducerDiscernibility="Object",
roc=FALSE, clroc="autism", fallBack=TRUE, fallBackClass="autism", maskFeatures=FALSE, maskFeaturesNames=c(),
underSample=FALSE, underSampleNum=0, underSampleSize=0, ruleFiltration=FALSE, ruleFiltrSupport=c(1,3),
ruleFiltrAccuracy=c(0,0.5), ruleFiltrCoverage=c(0,0), ruleFiltrStability=c(0,0),
JohnsonParam=list(Modulo=TRUE,BRT=FALSE,BRTprec=0.9,Precompute=FALSE,Approximate=TRUE,Fraction=0.95),
GeneticParam=list(Modulo=TRUE,BRT=FALSE,BRTprec=0.9,Precompute=FALSE,Approximate=TRUE,Fraction=0.95,Algorithm="Simple"),
ManualNames=c(), pAdjust=TRUE, pAdjustMethod="bonferroni", seed=1, invert=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt}{
A data frame containing decision table. The last column is decision. 
}
  \item{classifier}{
A character containing the classifier type: StandardVoter, ObjectTrackingVoter or NaiveBayesClassifier. Default is StandardVoter.
}
  \item{cvNum}{
A numeric value of the cross-validation number. Default is 10.
}
  \item{discrete}{
Logical. Set TRUE for discrete data. Default is FALSE.
}
  \item{discreteMethod}{
A character containing discretization method: EqualFrequency, MDL, Naive, SemiNaive or BROrthogonal.  Default is EqualFrequency.
}
  \item{discreteParam}{
A vector containing discretization parameters. May be of different length and values. See examples.
}
  \item{discreteMask}{
Logical. Set FALSE to disable discretization mask. Default is TRUE.
}
  \item{reducer}{
A character containing name of reducer method: Johnson, Genetic, Holte1R or Manual. Default is Johnson.
}
  \item{reducerDiscernibility}{
A character containing reducer discernibility option: Full or Object. Default is Object.
}
  \item{roc}{
Logical. Set TRUE to calculate the AUC and ROC values. Default is FALSE.
}
  \item{clroc}{
A character containing the name of the class. Default is "autism".
}
  \item{fallBack}{
Logical. Set TRUE to support classifier with fallback class. Default is TRUE.
}
  \item{fallBackClass}{
A character containing the name of the class. Default is "autism".
}
  \item{maskFeatures}{
Logical. Set TRUE to mask features during the classification process. Default is FALSE.
}
  \item{maskFeaturesNames}{
A character vector of the feature names to mask. Names shall correspond to the column names.
}
  \item{underSample}{
Logical. Set TRUE to perform undersampling. Default is FALSE. 
}
  \item{underSampleNum}{
The number of subsamples. For 0, minimum number of subsets that cover all the objects is selected. Default is 0.
}
  \item{underSampleSize}{
The size of each subsample. For 0, the size is taken from the smallest decision class. Default is 0.
}
  \item{ruleFiltration}{
Logical. Set TRUE to filter out rules. Default is FALSE.
}
  \item{ruleFiltrSupport}{
A vector of two integers containing interval of support values to filter out. Default is c(1,3).
}
  \item{ruleFiltrAccuracy}{
A vector of two numbers containing interval of accuracy values to filter out. Default is c(0,0.5).
}
  \item{ruleFiltrCoverage}{
A vector of two numbers containing interval of coverage values to filter out. Default is c(0,0).
}
  \item{ruleFiltrStability}{
A vector of two numbers containing interval of support values to filter out. Integer. Default is c(0,0).
}
  \item{JohnsonParam}{
A vector containing Johnson reducer parameters.
}
  \item{GeneticParam}{
A vector containing Genetic reducer parameters.
}
  \item{ManualNames}{
A vector containing manual names for manual reducer.
}
  \item{pAdjust}{
Logical. Set TRUE to apply p-value adjustment. Default is TRUE.
}
  \item{pAdjustMethod}{
A character containing the name of the method: holm, hochberg, hommel, bonferroni, BH, BY, fdr or none. Default is bonferroni.
}
    \item{seed}{
An integer. Seed to the random number generator. Default is 1.
}
      \item{invert}{
Logical.  Set TRUE to swap training for test set. Default is FALSE. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{out$main}{A data frame of rule table containing the information about: features, decision, accuracy, support, coverage, stability, p-value and other statistic. The table is decreasingly sorted according to the p-value.
}
    \item{out$quality}{A table of model quality: accuracy statistic, ROC and AUC measures.
}
    \item{out$usn}{An integer indicating the minimum number of required for subsets in undersampling.
}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Mateusz Garbulowski, Karolina Smolinska
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

###### default settings ######
resultsRos <- rosetta(autcon)
resultsRos$quality$accuracyMean

###### undersampling ######
resultsRosU <- rosetta(autcon, underSample=TRUE, underSampleNum=20, underSampleSize=10)
resultsRosU$quality$accuracyMean

###### classifiers ######
# StandardVoter
resultsRosSV <- rosetta(autcon, classifier="StandardVoter")
resultsRosSV$quality$accuracyMean

# ObjectTrackingVoter
resultsRosOTV <- rosetta(autcon, classifier="ObjectTrackingVoter")
resultsRosOTV$quality$accuracyMean

# NaiveBayesClassifier
resultsRosNB <- rosetta(autcon, classifier="NaiveBayesClassifier")
resultsRosNB$quality$accuracyMean

###### reducers ######
# Johnson
resultsRosJR <- rosetta(autcon, reducer="Johnson", JohnsonParam=c(Modulo=TRUE, BRT=TRUE,BRTprec=0.1,Precompute=FALSE,Approximate=TRUE,Fraction=0.9))
resultsRosJR$quality$accuracyMean

# Genetic
resultsRosGR <- rosetta(autcon, reducer="Genetic", GeneticParam=c(Modulo=TRUE, BRT=TRUE, BRTprec=0.1, Precompute=FALSE, Approximate=TRUE, Fraction=0.9, Algorithm="Simple"))
resultsRosGR$quality$accuracyMean

###### discernibility ######
# Full
resultsRosF <- rosetta(autcon, reducerDiscernibility="Full")
resultsRosF$quality$accuracyMean

# Object
resultsRosO <- rosetta(autcon, reducerDiscernibilit="Object")
resultsRosO$quality$accuracyMean

###### discretization ######
# EqualFrequencyScaler
resultsRosEF <- rosetta(autcon, discrete=FALSE, discreteMethod="EqualFrequency", discreteParam=3)
resultsRosEF$quality$accuracyMean

# MDL
resultsRosMDL <- rosetta(autcon, discrete=FALSE, discreteMethod="MDL")
resultsRosMDL$quality$accuracyMean

# Naive
resultsRosNaive <- rosetta(autcon, discrete=FALSE, discreteMethod="Naive")
resultsRosNaive$quality$accuracyMean

# SemiNaive
resultsRosSN <- rosetta(autcon, discrete=FALSE, discreteMethod="SemiNaive")
resultsRosSN$quality$accuracyMean

# BRO
resultsRosBR <- rosetta(autcon, discrete=FALSE, discreteMethod="BROrthogonal", discreteParam=list(TRUE,0.95))
resultsRosBR$quality$accuracyMean

###### for discrete data #######
# discretization is turned off
resultsRosNoD <- rosetta(autcon, discrete=TRUE)
resultsRosNoD$quality$accuracyMean
  
###### for mixed data: discrete and non-discrete #######
# data.frame should contain specific structures:
# discrete values: logical, character or factor
# non-discrete values: float, numeric or integer
resultsRosDM <- rosetta(autcon, discrete=FALSE)
resultsRosDM$quality$accuracyMean
  
###### calculate AUC #########  
# for class: autism
resultsRosAUCa <- rosetta(autcon, roc=TRUE, clroc="autism")
resultsRosAUCa$quality

# for class: control
resultsRosAUCc <- rosetta(autcon, roc=TRUE, clroc="control")
resultsRosAUCc$quality
  
# without ROC.AUC
resultsRosAUCa <- rosetta(autcon, roc= FALSE)
resultsRosAUCa$quality

###### set fallback class ######
#for class: autism
resultsRosFBa <- rosetta(autcon, fallBack=TRUE, fallBackClass="autism")
resultsRosFBa$quality$accuracyMean

#for class: control
resultsRosFBc <- rosetta(autcon, fallBack=TRUE, fallBackClass="control")
resultsRosFBc$quality$accuracyMean

###### rules filtration ######
# accuracy
resultsRosFA <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrAccuracy=c(0,0.75))
resultsRosFA$quality$accuracyMean

# support
resultsRosFS <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrSupport=c(1,10))
resultsRosF2$quality$accuracyMean

# coverage
resultsRosFC <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrCoverage=c(1,5))
resultsRosFC$quality$accuracyMean

# stability
resultsRosFST <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrStability=c(1,5))
resultsRosFST$quality$accuracyMean

###### mask features ######
resultsRosMF1 <- rosetta(autcon, maskFeatures=TRUE, maskFeaturesNames=c("COX2"))
resultsRosMF1$quality$accuracyMean

# remove first 10 features
resultsRosMF2 <- rosetta(autcon, maskFeatures=TRUE, maskFeaturesNames=colnames(autcon)[1:10])
resultsRosMF2$quality$accuracyMean
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
