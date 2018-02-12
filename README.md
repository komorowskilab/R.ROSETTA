<!--
  Title: RROSETTA
  Description: ROSETTA rough set classifier as a R package.
  Author: Mateusz Garbulowski
  -->
<meta name='keywords' content='rosetta, rough sets, classification'>

# RROSETTA

**ROSETTA** rough set classifier as a R package. In addition to all the existing ROSETTA (created by Öhrn and Komorowski in 1997) algorithms we have added new functions especially useful in bioinformatics applications. 
These include: 
* under-sampling
* p-value estimation of the rules
* clustering decision classes
* retrieving the support sets for rules
* reclassification

## Getting Started

### Prerequisites
RROSETTA works with windows and macOS platforms.<br />
For macOS systems wine is required.<br />
<a href="https://www.davidbaumgold.com/tutorials/wine-mac/" target="_blank">Here</a> is a tutorial by David Baumgold how to install wine in macOS platforms.

### Installing

Install and load devtools package
```R
install.packages("devtools")
library(devtools)
```

Install and load RROSETTA package from github
```R
install_github("mategarb/RROSETTA")
library(RROSETTA)
```
### Functions
**rosetta()** <-> *runs ROSETTA rough set classifier*<br />
**recalculateRules()** <-> *recalculates rules after undersampling and retrieves the support sets for rules*<br />
**saveLineByLine()** <-> *saves rules to LineByLine format, compatible with VisuNet http://bioinf.icm.uu.se/~visunet/*<br />
**getDecision()** <-> *reclassifies created model by applying rules*<br />
**getFeatures()** <-> *retrieves the most significant features from rules*<br />
**ruleHeatmap()** <-> *creates a heatmap per one rule*<br />

## Examples

All the examples you can find by typing ? and name of the function in R environment e.g.
```
?rosetta
```

## Acknowledgments
RROSETTA includes an example dataset obtained from GEO repository with the reference number [GSE25507](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE25507).


### Built With
* [ROSETTA](http://bioinf.icm.uu.se/rosetta/) - The ROSETTA framework
* [R Core](https://www.R-project.org/) - R Core Team
