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
The size of one subsample. If 0 minimum number from all groups is taken. Default is 0.
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
Interval of support values to filter out. Integer. Default is c(1,3).
}
  \item{ruleFiltrAccuracy}{
Interval of accuracy values to filter out. Float 0-1. Default is c(0.5,1).
}
  \item{ruleFiltrCoverage}{
Interval of coverage values to filter out. Float 0-1. Default is c(0,0).
}
  \item{ruleFiltrStability}{
Interval of support values to filter out. Integer. Default is c(0,0).
}
  \item{reducerDiscernibility}{
Reducer discernibility. Object or Full. Default is object.
}
  \item{JohnsonParam}{
Vector of Johnson reducer parameters.
}
  \item{GeneticParam}{
Vector of Genetic reducer parameters.
}
  \item{ManualNames}{
Manual names for reducer.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
output from rosetta:
out$main - data frame of rules with accuracy, support, coverage, stability and pvalue
out$statistic - accuracy, ROC, AUC values
out$rules - not sorted, raw rules
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

df=autism

###### DEFAULT ######
#Standard Voter, Johnson Reducer, 10 CV, Equal Frequency 
resultsRos=rosetta(df) 
resultsRos$statistic$Accuracy

###### UNDERSAMPLING ######
##undersampling 20 undersamples, 50 size of class 
resultsRosU1=rosetta(df, underSample=TRUE, underSampleNum=20, underSampleSize=10)
resultsRosU1$statistic$Accuracy

##undersampling 20 undersamples, size of the class set to 0 (as deafault)
resultsRosU2=rosetta(df, underSample=TRUE, underSampleNum=20, underSampleSize=0)
resultsRosU2$statistic$Accuracy

###### CLASSIFIERS ######
#StandardVoter
resultsRosSV=rosetta(df, classifier="StandardVoter")
resultsRosSV$statistic$Accuracy

#ObjectTrackingVoter
resultsRosOTV=rosetta(df, classifier="ObjectTrackingVoter")
resultsRosOTV$statistic$Accuracy

#NaiveBayesClassifier
resultsRosNB=rosetta(df, classifier="NaiveBayesClassifier")
resultsRosNB$statistic$Accuracy

###### DISCERNIBILITY ######
##Full
resultsRosF=rosetta(df, reducerDiscernibility="Full")
resultsRosF$statistic$Accuracy

##Object
resultsRosO=rosetta(df, reducerDiscernibilit="Object")
resultsRosO$statistic$Accuracy

###### REDUCERS ######
##Johnson
resultsRosJR=rosetta(df, reducer="Johnson", JohnsonParam=c(Modulo=TRUE, BRT=TRUE,BRTprec=0.1,Precompute=FALSE,Approximate=TRUE,Fraction=0.9))
resultsRosJR$statistic$Accuracy

##Genetic
resultsRosGR=rosetta(df, reducer="Genetic", GeneticParam=c(Modulo=TRUE, BRT=TRUE, BRTprec=0.1, Precompute=FALSE, Approximate=TRUE, Fraction=0.85, Algorithm="Simple"))
resultsRosGR$statistic$Accuracy


##Holte1R
resultsRosHR=rosetta(df, reducer="Holte1R")
resultsRosHR$statistic$Accuracy

##Manual
resultsRosMR=rosetta(df, reducer="Manual", ManualNames = c("DDX60L", "SENCR", "ZSCAN18"))
resultsRosMR$statistic$Accuracy

######## DISCRETIZATION ########
## EqualFrequencyScaler
resultsRosEF=rosetta(df, discrete=TRUE, discreteMethod="EqualFrequency", discreteParam=3)
resultsRosEF$Accuracy

## MDL
resultsRosMDL=rosetta(df, discrete=TRUE, discreteMethod="MDL")
resultsRosMDL$statistic$Accuracy

## NAIVE
resultsRosNaive=rosetta(df, discrete=TRUE, discreteMethod="Naive")
resultsRosNaive$statistic$Accuracy

##SemiNaive
resultsRosSN=rosetta(df, discrete=TRUE, discreteMethod="SemiNaive")
resultsRosSN$statistic$Accuracy

## BRO
resultsRosBR=rosetta(df, discrete=TRUE, discreteMethod="BROrthogonal", discreteParam=c(T,0.95))
resultsRosBR$statistic$Accuracy

## without discretization mask
resultsRosMask=rosetta(df, discrete=TRUE, discreteMethod="EqualFrequency", discreteParam=3, discreteMask=F)
resultsRosMask$statistic$Accuracy

## no discretization
resultsRosNoD=rosetta(df, discrete=FALSE)
resultsRosNoD$statistic$Accuracy

#### RULES FILTRATION ####

#accuracy
resultsRosFA=rosetta(df, ruleFiltration=TRUE, ruleFiltrAccuracy=c(0.2,0.6))
resultsRosFA$statistic$Accuracy

#support
resultsRosFS=rosetta(df, ruleFiltration=TRUE, ruleFiltrSupport=c(1,5))
resultsRosF2$statistic$Accuracy

#coverage
resultsRosFC=rosetta(df, ruleFiltration=TRUE, ruleFiltrCoverage=c(1,5))
resultsRosFC$statistic$Accuracy

#stability
resultsRosFST=rosetta(df, ruleFiltration=TRUE, ruleFiltrStability=c(1,5))
resultsRosFST$statistic$Accuracy


#### MASK FEATURES ####
resultsRosMF=rosetta(df)
resultsRosMF$statistic$Accuracy

resultsRosMF1=rosetta(df, maskFeatures=TRUE, maskFeaturesNames=c("MAP7"))
resultsRosMF1$statistic$Accuracy

resultsRosMF3=rosetta(df, maskFeatures=TRUE, maskFeaturesNames=c("DDX60L", "SENCR", "ZSCAN18"))
resultsRosMF3$statistic$Accuracy
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
