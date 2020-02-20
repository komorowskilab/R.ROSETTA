\name{rosetta}
\alias{rosetta}
\title{
ROSETTA classifier.
}
\description{
Performs a rule-based classification.
}
\usage{
rosetta(dt, classifier = "StandardVoter", cvNum = 10, discrete = FALSE, discreteMethod = "EqualFrequency",
discreteParam = 3, discreteMask = TRUE, reducer = "Johnson", reducerDiscernibility = "Object",
roc = FALSE, clroc = "autism", fallBack = TRUE, fallBackClass = "autism", maskFeatures = FALSE, maskFeaturesNames = c(),
underSample = FALSE, underSampleNum = 0, underSampleSize = 0, ruleFiltration = FALSE, ruleFiltrSupport = c(1, 3),
ruleFiltrAccuracy = c(0, 0.5), ruleFiltrCoverage = c(0, 0), ruleFiltrStability = c(0, 0),
JohnsonParam = list(Modulo=TRUE, BRT=FALSE, BRTprec=0.9, Precompute=FALSE, Approximate=TRUE, Fraction=0.95),
GeneticParam = list(Modulo=TRUE, BRT=FALSE, BRTprec=0.9, Precompute=FALSE, Approximate=TRUE, Fraction=0.95, Algorithm="Simple"),
ManualNames = c(), pAdjust = TRUE, pAdjustMethod = "bonferroni", seed = 1, invert = FALSE, fraction=0.5, calibration = FALSE,
fillNA = FALSE, fillNAmethod = "meanOrMode", remSpChars = FALSE)
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
A character containing name of reducer method: Johnson or Genetic. Default is Johnson.
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
The number of subset for undersampling. For 0, minimum number of subsets that cover all the objects is selected. Default is 0.
}
  \item{underSampleSize}{
The size of each subset for undersampling. For 0, the size is taken from the smallest decision class. Default is 0.
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
Logical. Set TRUE to apply rule p-value and relative risk p-value adjustment. Default is TRUE.
}
  \item{pAdjustMethod}{
A character containing the name of the method: holm, hochberg, hommel, bonferroni, BH, BY, fdr or none. Default is bonferroni.
}
    \item{seed}{
An integer. Seed to the random number generator. Default is 1.
}
      \item{invert}{
Logical. Set TRUE to swap training for test set. Default is FALSE. 
}
      \item{fraction}{
Numeric. Hitting fraction for classifier.
}
      \item{calibration}{
Logical. Set TRUE for calibration.
}
      \item{fillNA}{
Logical. Set TRUE to fill NA values.
}
      \item{fillNAmethod}{
Character. Set method of filling NA values: meanOrMode or combinatorial. 
}
      \item{remSpChars}{
Logical. Remove special characters from feature names. Default is FALSE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{main}{A data frame containing rule information about: features, discretization levels, decision, accuracy, support, coverage, stability, p-value and other statistic. The table is decreasingly sorted according to the p-value.
}
    \item{quality}{A table of model quality: accuracy statistic, ROC and AUC measures.
}
    \item{usMeanAccs}{A vector containing accuracies of the models from undersampling. Only if underSample = TRUE.
}
    \item{usn}{An integer indicating the minimum number of required subsets for undersampling. Only if underSample = TRUE.
}
    \item{ROCstats}{A data frame containing statistic of the model: 1 - specificity, sensitivity, specificity, PPV, NPV, accuracy and threshold. Only if roc = TRUE.
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

### default settings ###
ruleModel <- rosetta(autcon)
ruleModel$quality$accuracyMean

### undersampling ###
ruleModelUS <- rosetta(autcon, underSample=TRUE, underSampleNum=10, underSampleSize=50)
ruleModelUS$quality$accuracyMean

### classifiers ###
# StandardVoter
ruleModelSV <- rosetta(autcon, classifier="StandardVoter")
ruleModelSV$quality$accuracyMean

# ObjectTrackingVoter
ruleModelOTV <- rosetta(autcon, classifier="ObjectTrackingVoter")
ruleModelOTV$quality$accuracyMean

# NaiveBayesClassifier
ruleModelNBC <- rosetta(autcon, classifier="NaiveBayesClassifier")
ruleModelNBC$quality$accuracyMean

### reducers ###
# Johnson
ruleModelJohnson <- rosetta(autcon, reducer="Johnson", JohnsonParam=c(Modulo=TRUE, BRT=TRUE, BRTprec=0.1, Precompute=FALSE, Approximate=TRUE, Fraction=0.8))
ruleModelJohnson$quality$accuracyMean

# Genetic
ruleModelGenetic <- rosetta(autcon, reducer="Genetic", GeneticParam=c(Modulo=TRUE, BRT=TRUE, BRTprec=0.1, Precompute=FALSE, Approximate=TRUE, Fraction=0.8, Algorithm="Simple"))
ruleModelGenetic$quality$accuracyMean

### discernibility ###
# Full
ruleModelFull <- rosetta(autcon, reducerDiscernibility="Full")
ruleModelFull$quality$accuracyMean

# Object
ruleModelObject <- rosetta(autcon, reducerDiscernibilit="Object")
ruleModelObject$quality$accuracyMean

### discretization ###
# EqualFrequencyScaler
ruleModelEF <- rosetta(autcon, discrete=FALSE, discreteMethod="EqualFrequency", discreteParam=3)
ruleModelEF$quality$accuracyMean

# MDL
ruleModelMDL <- rosetta(autcon, discrete=FALSE, discreteMethod="MDL")
ruleModelMDL$quality$accuracyMean

# Naive
ruleModelNaive <- rosetta(autcon, discrete=FALSE, discreteMethod="Naive")
ruleModelNaive$quality$accuracyMean

# SemiNaive
ruleModelSemiNaive <- rosetta(autcon, discrete=FALSE, discreteMethod="SemiNaive")
ruleModelSemiNaive$quality$accuracyMean

# BRO
ruleModelBRO <- rosetta(autcon, discrete=FALSE, discreteMethod="BROrthogonal", discreteParam=list(TRUE, 0.95))
ruleModelBRO$quality$accuracyMean

### for discrete data ###
# generate discrete synthetic data
dt <- synData(nFeatures=c(5,5,3,2,2), rf=c(0.2,0.3,0.2,0.4,0.4), 
              rd=c(0.2,0.3,0.4,0.5,0.6), discrete = TRUE, levels = 3, labels = c("low", "medium", "high"))
ruleModelDiscrete <- rosetta(dt, discrete = TRUE)
ruleModelDiscrete$quality$accuracyMean

### for mixed data(discrete and non-discrete) data frame should contain specific structures: ###
# for discrete values: logical, character or factor
# for non-discrete values: float, numeric or integer

# generate continouous synthetic data
dt <- synData(nFeatures=c(20,2,2,3,3), rf=c(0.1,0.1,0.1,0.8,0.8), rd=c(0.5,0.1,0.7,0.8,0.2), nObjects=100, nOutcome=2, unbalanced=F, seed=1)
# change two of the features from the group 5 to discrete
dt$f5.2_rf0.8_rd0.2 <- as.factor(cut(dt$f5.2_rf0.8_rd0.2, 3, labels = c("low", "medium", "high")))
dt$f5.3_rf0.8_rd0.2 <- as.factor(cut(dt$f5.3_rf0.8_rd0.2, 3, labels = c("low", "medium", "high")))
ruleModelMixed <- rosetta(dt, discrete=F)
ruleModelMixed$quality$accuracyMean

### calculate AUC ###
# for class: autism
ruleModelAUCa <- rosetta(autcon, roc=TRUE, clroc="autism")
ruleModelAUCa$quality

# for class: control
ruleModelAUCc <- rosetta(autcon, roc=TRUE, clroc="control")
ruleModelAUCc$quality

### set fallback class ###
#for class: autism
ruleModelFBa <- rosetta(autcon, fallBack=TRUE, fallBackClass="autism")
ruleModelFBa$quality$accuracyMean

#for class: control
ruleModelFBc <- rosetta(autcon, fallBack=TRUE, fallBackClass="control")
ruleModelFBc$quality$accuracyMean

### rules filtration ###
# accuracy
ruleModelFiltAcc <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrAccuracy=c(0, 0.85))
ruleModelFiltAcc$quality$accuracyMean

# support 
ruleModelFiltSupp <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrSupport=c(1, 10))
ruleModelFiltSupp$quality$accuracyMean

# coverage 
ruleModelFiltCov <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrCoverage=c(0, 0.1))
ruleModelFiltCov$quality$accuracyMean

# stability
ruleModelFiltStab <- rosetta(autcon, ruleFiltration=TRUE, ruleFiltrStability=c(1, 5))
dim(ruleModelFiltStab$main)[1]

### mask features ###
ruleModelMaskFs2 <- rosetta(autcon, maskFeatures=TRUE, maskFeaturesNames=c("MAP7", "COX2"))
ruleModelMaskFs2$quality$accuracyMean

# remove first 10 features from decision table
ruleModelMaskFs10 <- rosetta(autcon, maskFeatures=TRUE, maskFeaturesNames=colnames(autcon)[1:10])
ruleModelMaskFs10$quality$accuracyMean

### fill NA values ###
autcon2 <- autcon
#introduce 3 NA values
autcon2[2,2] <- NA
autcon2[3,3] <- NA
autcon2[4,4] <- NA
ruleModelFillNA <- rosetta(autcon2, fillNA=TRUE, fillNAmethod="meanOrMode")
ruleModelFillNA$quality$accuracyMean

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
