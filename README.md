<!--
  Title: RROSETTA
  Description: ROSETTA rough set classifier as a R package.
  Author: Mateusz Garbulowski
  -->
<meta name='keywords' content='rosetta, rough sets, classification'>

# R.ROSETTA

**ROSETTA** create rough set cassification models. The R.ROSETTA is an R package based on ROSETTA(Öhrn and Komorowski, 1997) software. In addition to all the existing ROSETTA functionalities, we have added new functions especially useful in bioinformatic applications. 
These include: 
* under-sampling
* rule p-value estimation
* retrieving the rule support sets
* class prediction
* model visualization as a rule network or heatmap

## Getting Started

### Prerequisites
R.ROSETTA works with UNIX and windows operating systems.<br />
For unix systems wine is required.<br />
<a href="https://www.davidbaumgold.com/tutorials/wine-mac/" target="_blank">A tutorial</a> by David Baumgold, how to install wine on macOS platforms.

### Installation

Installation from github requires devtools package:
```R
install.packages("devtools")
```

Installation and loading R.ROSETTA package from github:
```R
library(devtools)
install_github("mategarb/R.ROSETTA")

library(R.ROSETTA)
```
### Functions
**rosetta()** <-> *create ROSETTA rough set classification model*<br />
**recalculateRules()** <-> *recalculate rule table using the input data. Retrieve the support sets for rules*<br />
**predictClass()** <-> *predicts new classes by applying the model*<br />
**getFeatures()** <-> *retrieves the attributes from the rules*<br />
**saveLineByLine()** <-> *saves rules to LineByLine text format, compatible with VisuNet http://bioinf.icm.uu.se/~visunet/ or Ciruvis http://bioinf.icm.uu.se/~ciruvis/*<br />
**ruleHeatmap()** <-> *creates a heatmap of object-attribute interactions*<br />

## Examples

All the examples can be found in the R function manuals. To see the description, type "?" and the name of the function in R environment e.g.
```
?rosetta, ?saveLineByLine
```

## Acknowledgments
R.ROSETTA includes an example dataset obtained from GEO repository with the reference number [GSE25507](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE25507).


### Built With
* [ROSETTA](http://bioinf.icm.uu.se/rosetta/) - The ROSETTA framework
* [R Core](https://www.R-project.org/) - R Core Team
