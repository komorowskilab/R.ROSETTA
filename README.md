<!--
  Title: RROSETTA
  Description: ROSETTA rough set classifier as a R package.
  Author: Mateusz Garbulowski
  -->
<meta name='keywords' content='rosetta, rough sets, classification'>

# R.ROSETTA

**R.ROSETTA** is an R package for constructing and analyzing rule-based classification models. R.ROSETTA is designed to support the overall data mining and knowledge discovery process. The presented tool is a more accessible and extended version of ROSETTA (Öhrn and Komorowski, 1997) system. In addition to all the existing ROSETTA functionalities, we have added new functions such as:
* undersampling
* rule p-value estimation
* retrieving support sets from rules
* class prediction
* model visualization on a rule network or heatmap

## Getting Started

### Prerequisites
R.ROSETTA works with UNIX and Windows OS.<br />
UNIX operating systems require wine.<br />
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
**rosetta()** <-> *generates rough set-based classification model*<br />
**recalculateRules()** <-> *recalculates rules using the input data. Retrieves the support sets for rules*<br />
**predictClass()** <-> *predicts new classes by applying the model*<br />
**getFeatures()** <-> *retrieves the attributes from the rules*<br />
**saveLineByLine()** <-> *saves rules to LineByLine text format, compatible with VisuNet http://bioinf.icm.uu.se/~visunet/ or Ciruvis http://bioinf.icm.uu.se/~ciruvis/*<br />
**ruleHeatmap()** <-> *creates a heatmap of object-attribute interactions for a single rule*<br />
**viewRules()** <-> *displays rules in IF-THEN format*<br />
**randRules()** <-> *generates a set of random rules*<br />

## Examples

Sample code can be found in the R function manuals. To see the description, type "?" and the name of the function e.g.
```
?rosetta, ?saveLineByLine
```

## Acknowledgments
R.ROSETTA includes a sample dataset collected from GEO repository with the reference number [GSE25507](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE25507).


### Built With
* [ROSETTA](http://bioinf.icm.uu.se/rosetta/) - The ROSETTA framework
* [R Core](https://www.R-project.org/) - R Core Team
