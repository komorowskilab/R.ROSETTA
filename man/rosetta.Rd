\name{rosetta}
\alias{rosetta}
rosetta
\title{
ROSETTA classifier
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
Decision table in a form of data frame. The last column has to contain decisions.
}
  \item{classifier}{
The type of classifier: StandardVoter, ObjectTrackingVoter or NaiveBayesClassifier. 
}
  \item{cvNum}{
The number of cross validation. Default is set to 10.
}
  \item{discrete}{
Set to TRUE if your data is not discretized. Default TRUE.
}
  \item{discreteMethod}{
Discretization methods: EqualFrequency, MDL, Naive, SemiNaive or BROrthogonal.
}
  \item{discreteParam}{
Discretization parameters. Vector of parameters.
}
  \item{discreteMask}{
Set the discretization mask. Default TRUE.
}
  \item{reducer}{
Reducer methods: Johnson, Genetic, Holte1R or Manual.
}
  \item{maskFeatures}{
Set to TRUE if you want mask some features. Default is FALSE.
}
  \item{maskFeaturesNames}{
Vector of names of the features to mask.
}
  \item{underSample}{
Set to TRUE if your data is not balanced. 
}
  \item{underSampleNum}{
The number of subsamples. Set to 0 if you want to caluclate minimum number of subsets that cover all the objects. Default is 0.
}
  \item{underSampleSize}{
The size of one subsample. Set to 0 if you want to take the minimum of all subsets. Default is 0.
}
  \item{fallBack}{
Fallback. Default is TRUE.
}
  \item{fallBackClass}{
Full or Object. Default is 0.
}
  \item{ruleFiltration}{
Rule filtration. True or False.
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
library(rROSETTA)

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
