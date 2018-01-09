\name{rosetta}
\alias{rosetta}
\title{
ROSETTA rough set classifier
}
\description{
Run ROSETTA rough set classifier. In addition to all the existing ROSETTA algorithms we have added new functions especially useful in bioinformatics applications.  These include: under-sampling, p-value estimation of the rules, clustering decision classes and retrieving the true support sets for rules.
}
\usage{
rosetta(df, classifier = "StandardVoter", cvNum = 10, discrete = TRUE, discreteMethod = "EqualFrequencyScaler", discreteParam = 3, discreteMask = TRUE, reducer = "JohnsonReducer", maskFeatures = FALSE, maskFeaturesNames = c(), underSample = FALSE, underSampleNum = 0, underSampleSize = 0, underSampleLevel = 2, fallBack = TRUE, fallBackClass = 0, ruleFiltration = TRUE, ruleFiltrSupport = c(1, 3), ruleFiltrAccuracy = c(0, 0.5), ruleFiltrCoverage = c(0, 0), ruleFiltrStability = c(0, 0), reducerDiscernibility = "Object", JohnsonParam = c(Modulo = TRUE, BRT = FALSE, BRTprec = 0.9, Precompute = FALSE, Approximate = TRUE, Fraction = 0.9), GeneticParam = c(Modulo = TRUE, BRT = FALSE, BRTprec = 0.9, Precompute = FALSE, Approximate = TRUE, Fraction = 0.9, Algorithm = "Simple"), ManualNames = c())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame containing decision table. The last column should contain decisions.
}
  \item{classifier}{
A character containing the type of classifier: StandardVoter, ObjectTrackingVoter or NaiveBayesClassifier. Default is StandardVoter.
}
  \item{cvNum}{
The number of cross validation. Default is 10.
}
  \item{discrete}{
Logical. If TRUE, your data will be discretized in ROSETTA. Set FALSE when your data is already discretized. Default is TRUE.
}
  \item{discreteMethod}{
A character containing discretization method: EqualFrequency, MDL, Naive, SemiNaive or BROrthogonal.  Default is EqualFrequency.
}
  \item{discreteParam}{
A vector containing discretization parameters. May be of different length. See examples in rosetta.
}
  \item{discreteMask}{
Logical. If TRUE, set the discretization mask option. Default is TRUE.
}
  \item{reducer}{
A character containing reducer method: Johnson, Genetic, Holte1R or Manual. Default is Johnson.
}
  \item{maskFeatures}{
Logical. If TRUE, function mask features chosen by user. Default is FALSE.
}
  \item{maskFeaturesNames}{
A vector of feature names to mask. Names should be the same as ones in column names.
}
  \item{underSample}{
Logical. If TRUE, the algorithm will use under-sampling. Use if your data is not balanced. Default is FALSE. 
}
  \item{underSampleNum}{
The number of sub-samples. If 0 minimum number of subsets that cover all the objects is taken. Default is 0.
}
  \item{underSampleSize}{
The size of each subsample. If 0 minimum number from all groups is taken. Default is 0.
}
  \item{fallBack}{
Logical. If TRUE, fallback is on. Default is TRUE.
}
  \item{fallBackClass}{
A character containing fallback option: Full or Object. Default is Full.
}
  \item{ruleFiltration}{
Logical. If TRUE, rule filtration is on. Default is TRUE.
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
  \item{reducerDiscernibility}{
A character containing reducer discernibility option: Full or Object. Default is Object.

}
  \item{JohnsonParam}{
A vector containing Johnson reducer parameters.
}
  \item{GeneticParam}{
A vector containing of Genetic reducer parameters.
}
  \item{ManualNames}{
A vector containing manual names for manual reducer.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
output from rosetta:
out$main - a data frame sorted by rule pvalue containing rules information such as: features, decision, accuracy, support, coverage, stability and pvalue
out$statistic - accuracy, ROC, AUC values
out$rules - raw rules that are not sorted
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
library(RROSETTA)

#example data loading
df=autism

###### dedault parameters ######
resultsRos=rosetta(df) 
resultsRos$statistic$Accuracy

###### under-sampling ######
resultsRosU=rosetta(df, underSample=TRUE, underSampleNum=20, underSampleSize=10)
resultsRosU$statistic$Accuracy

###### under-sampling with default parameters ######
resultsRosUD=rosetta(df, underSample=TRUE)
resultsRosUD$statistic$Accuracy

###### classifiers ######
# StandardVoter
resultsRosSV=rosetta(df, classifier="StandardVoter")
resultsRosSV$statistic$Accuracy

# ObjectTrackingVoter
resultsRosOTV=rosetta(df, classifier="ObjectTrackingVoter")
resultsRosOTV$statistic$Accuracy

# NaiveBayesClassifier
resultsRosNB=rosetta(df, classifier="NaiveBayesClassifier")
resultsRosNB$statistic$Accuracy

###### reducers ######
# Johnson
resultsRosJR=rosetta(df, reducer="Johnson", JohnsonParam=c(Modulo=TRUE, BRT=TRUE,BRTprec=0.1,Precompute=FALSE,Approximate=TRUE,Fraction=0.9))
resultsRosJR$statistic$Accuracy

# Genetic
resultsRosGR=rosetta(df, reducer="Genetic", GeneticParam=c(Modulo=TRUE, BRT=TRUE, BRTprec=0.1, Precompute=FALSE, Approximate=TRUE, Fraction=0.85, Algorithm="Simple"))
resultsRosGR$statistic$Accuracy

# Holte1R
resultsRosHR=rosetta(df, reducer="Holte1R")
resultsRosHR$statistic$Accuracy

# Manual
resultsRosMR=rosetta(df, reducer="Manual", ManualNames = c("DDX60L", "SENCR", "ZSCAN18"))
resultsRosMR$statistic$Accuracy

###### discretization ######
# EqualFrequencyScaler
resultsRosEF=rosetta(df, discrete=TRUE, discreteMethod="EqualFrequency", discreteParam=3)
resultsRosEF$Accuracy

# MDL
resultsRosMDL=rosetta(df, discrete=TRUE, discreteMethod="MDL")
resultsRosMDL$statistic$Accuracy

# Naive
resultsRosNaive=rosetta(df, discrete=TRUE, discreteMethod="Naive")
resultsRosNaive$statistic$Accuracy

# SemiNaive
resultsRosSN=rosetta(df, discrete=TRUE, discreteMethod="SemiNaive")
resultsRosSN$statistic$Accuracy

# BRO
resultsRosBR=rosetta(df, discrete=TRUE, discreteMethod="BROrthogonal", discreteParam=c(T,0.95))
resultsRosBR$statistic$Accuracy

# without discretization mask
resultsRosMask=rosetta(df, discrete=TRUE, discreteMask=F)
resultsRosMask$statistic$Accuracy

###### no discretization ######
# for discrete data
resultsRosNoD=rosetta(df, discrete=FALSE)
resultsRosNoD$statistic$Accuracy

###### rules filtration ######
# accuracy
resultsRosFA=rosetta(df, ruleFiltration=TRUE, ruleFiltrAccuracy=c(0,0.75))
resultsRosFA$statistic$Accuracy

# support
resultsRosFS=rosetta(df, ruleFiltration=TRUE, ruleFiltrSupport=c(1,10))
resultsRosF2$statistic$Accuracy

# coverage
resultsRosFC=rosetta(df, ruleFiltration=TRUE, ruleFiltrCoverage=c(1,5))
resultsRosFC$statistic$Accuracy

# stability
resultsRosFST=rosetta(df, ruleFiltration=TRUE, ruleFiltrStability=c(1,5))
resultsRosFST$statistic$Accuracy

###### mask features ######
resultsRosMF1=rosetta(df, maskFeatures=TRUE, maskFeaturesNames=c("MAP7"))
resultsRosMF1$statistic$Accuracy

resultsRosMF3=rosetta(df, maskFeatures=TRUE, maskFeaturesNames=c("DDX60L", "SENCR", "ZSCAN18"))
resultsRosMF3$statistic$Accuracy

###### discernibility ######
# Full
resultsRosF=rosetta(df, reducerDiscernibility="Full")
resultsRosF$statistic$Accuracy

# Object
resultsRosO=rosetta(df, reducerDiscernibilit="Object")
resultsRosO$statistic$Accuracy

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
