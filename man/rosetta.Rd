\name{rosetta}
\alias{rosetta}
\title{
ROSETTA rough set classifier
}
\description{
Creates a rough set classification model.
}
\usage{
rosetta(dt, classifier="StandardVoter", cvNum=10, discrete=FALSE, discreteMethod="EqualFrequency",
discreteParam=3, discreteMask=TRUE, reducer="Johnson", reducerDiscernibility="Object",
roc=FALSE, clroc="autism", fallBack=TRUE, fallBackClass="autism", maskFeatures=FALSE, maskFeaturesNames=c(),
underSample=FALSE, underSampleNum=0, underSampleSize=0, ruleFiltration=FALSE, ruleFiltrSupport=c(1,3),
ruleFiltrAccuracy=c(0,0.5), ruleFiltrCoverage=c(0,0), ruleFiltrStability=c(0,0),
JohnsonParam=c(Modulo=TRUE,BRT=FALSE,BRTprec=0.9,Precompute=FALSE,Approximate=TRUE,Fraction=0.95),
GeneticParam=c(Modulo=TRUE,BRT=FALSE,BRTprec=0.9,Precompute=FALSE,Approximate=TRUE,Fraction=0.95,Algorithm="Simple"),
ManualNames=c(), pAdjust=TRUE, pAdjustMethod="BH", seed=1, invert=FALSE)
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
Logical. Set TRUE for non-discrete data. Default is FALSE.
}
  \item{discreteMethod}{
A character containing discretization method: EqualFrequency, MDL, Naive, SemiNaive or BROrthogonal.  Default is EqualFrequency.
}
  \item{discreteParam}{
A vector containing discretization parameters. May be of different length and values. See examples in rosetta.
}
  \item{discreteMask}{
Logical. If TRUE, set the discretization mask option. Default is TRUE.
}
  \item{reducer}{
A character containing reducer method: Johnson, Genetic, Holte1R or Manual. Default is Johnson.
}
  \item{reducerDiscernibility}{
A character containing reducer discernibility option: Full or Object. Default is Object.
}
  \item{roc}{
Logical. If TRUE, calculate AUC and ROC values. Default is FALSE.
}
  \item{clroc}{
A character containing the name of the class. Default is "autism".
}
  \item{fallBack}{
Logical. If TRUE, fallback class is supported. Default is TRUE.
}
  \item{fallBackClass}{
A character containing the name of the class. Default is "autism".
}
  \item{maskFeatures}{
Logical. If TRUE, masks features during the classification process. Default is FALSE.
}
  \item{maskFeaturesNames}{
A character vector of the feature names to mask. Names should correspond to the column names.
}
  \item{underSample}{
Logical. If TRUE, the under-sampling is performed. Default is FALSE. 
}
  \item{underSampleNum}{
The number of sub-samples. If 0 minimum number of subsets that cover all the objects is taken. Default is 0.
}
  \item{underSampleSize}{
The size of each subsample. If 0 the size is taken from the smallest decision class. Default is 0.
}
  \item{ruleFiltration}{
Logical. If TRUE, rule filtration is performed. Default is FALSE.
}
  \item{ruleFiltrSupport}{
A vector of two numbers containing interval of support values to filter out. Default is c(1,3).
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
Logical. If TRUE, p-value adjustment is applied. Default is TRUE.
}
  \item{pAdjustMethod}{
A character containing the name of the method: holm, hochberg, hommel, bonferroni, BH, BY, fdr or none.
}
    \item{seed}{
An integer. Seed to the random number generator.
}
      \item{invert}{
Logical.  If TRUE, the classifier is inverted. Default is FALSE. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{out$main}{A data frame of rule table containing an information of: features, decision, accuracy, support, coverage, stability and p-value. The table is sorted according to the p-value.
}
    \item{out$quality}{A table of quality of the model: accuracy, ROC and AUC measures.
}
    \item{out$rules}{A character vector of raw rules computed by ROSETTA.
}
    \item{out$usn}{An integer indicating the minimum number of required for subsets in undersumpling.
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
resultsRos$quality$Accuracy.Mean

###### under-sampling ######
resultsRosU <- rosetta(autcon, underSample=TRUE, underSampleNum=20, underSampleSize=10)
resultsRosU$quality$Accuracy.Mean

###### under-sampling with default parameters ######
resultsRosUD <- rosetta(autcon, underSample=TRUE)
resultsRosUD$quality$Accuracy.Mean

###### classifiers ######
# StandardVoter
resultsRosSV <- rosetta(autcon, classifier="StandardVoter")
resultsRosSV$quality$Accuracy.Mean

# ObjectTrackingVoter
resultsRosOTV <- rosetta(autcon, classifier="ObjectTrackingVoter")
resultsRosOTV$quality$Accuracy.Mean

# NaiveBayesClassifier
resultsRosNB <- rosetta(autcon, classifier="NaiveBayesClassifier")
resultsRosNB$quality$Accuracy.Mean

###### reducers ######
# Johnson
resultsRosJR <- rosetta(autcon, reducer="Johnson", JohnsonParam=c(Modulo=TRUE, BRT=TRUE,BRTprec=0.1,Precompute=FALSE,Approximate=TRUE,Fraction=0.9))
resultsRosJR$quality$Accuracy.Mean

# Genetic
resultsRosGR <- rosetta(autcon, reducer="Genetic", GeneticParam=c(Modulo=TRUE, BRT=TRUE, BRTprec=0.1, Precompute=FALSE, Approximate=TRUE, Fraction=0.85, Algorithm="Simple"))
resultsRosGR$quality$Accuracy.Mean

###### discernibility ######
# Full
resultsRosF <- rosetta(autcon, reducerDiscernibility="Full")
resultsRosF$quality$Accuracy.Mean

# Object
resultsRosO <- rosetta(autcon, reducerDiscernibilit="Object")
resultsRosO$quality$Accuracy.Mean

###### discretization ######
# EqualFrequencyScaler
resultsRosEF <- rosetta(autcon, discrete=FALSE, discreteMethod="EqualFrequency", discreteParam=3)
resultsRosEF$quality$Accuracy.Mean

# MDL
resultsRosMDL <- rosetta(autcon, discrete=FALSE, discreteMethod="MDL")
resultsRosMDL$quality$Accuracy.Mean

# Naive
resultsRosNaive <- rosetta(autcon, discrete=FALSE, discreteMethod="Naive")
resultsRosNaive$quality$Accuracy.Mean

# SemiNaive
resultsRosSN <- rosetta(autcon, discrete=FALSE, discreteMethod="SemiNaive")
resultsRosSN$quality$Accuracy.Mean

# BRO
resultsRosBR <- rosetta(autcon, discrete=FALSE, discreteMethod="BROrthogonal", discreteParam=c(T,0.95))
resultsRosBR$quality$Accuracy.Mean

# without discretization mask
resultsRosMask <- rosetta(autcon, discrete=FALSE, discreteMask=FALSE)
resultsRosMask$quality$Accuracy.Mean

###### for discrete data e.g. binary values or characters ######
# discretization is turned off
resultsRosNoD <- rosetta(autcon, discrete=TRUE)
resultsRosNoD$quality$Accuracy.Mean

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
resultsRosFBa$quality$Accuracy.Mean

#for class: control
resultsRosFBc <- rosetta(autcon, fallBack=TRUE, fallBackClass="control")
resultsRosFBc$quality$Accuracy.Mean

###### rules filtration ######
# accuracy
resultsRosFA <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrAccuracy=c(0,0.75))
resultsRosFA$quality$Accuracy.Mean

# support
resultsRosFS <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrSupport=c(1,10))
resultsRosF2$quality$Accuracy.Mean

# coverage
resultsRosFC <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrCoverage=c(1,5))
resultsRosFC$quality$Accuracy.Mean

# stability
resultsRosFST <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrStability=c(1,5))
resultsRosFST$quality$Accuracy.Mean

###### mask features ######
resultsRosMF1 <- rosetta(autcon, maskFeatures=TRUE, maskFeaturesNames=c("COX2"))
resultsRosMF1$quality$Accuracy.Mean

# remove first 20 features
resultsRosMF2 <- rosetta(autcon, maskFeatures=TRUE, maskFeaturesNames=colnames(df)[1:20])
resultsRosMF2$quality$Accuracy.Mean
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
