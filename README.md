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

## Functions
**rosetta()** <-> *runs ROSETTA rough set classifier*<br />
**recalculateRules()** <-> *recalculates rules after undersampling and retrieves the support sets for rules*<br />
**saveLineByLine()** <-> *saves rules to Line By Line format, compatible with VisuNet http://bioinf.icm.uu.se/~visunet/*<br />
**getDecision()** <-> *reclassifies created model by applying rules*<br />

## Installation

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

## Example data
RROSETTA includes an example dataset obtained from GEO repository with the reference number [GSE25507](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE25507).


## Built With

* [ROSETTA](http://bioinf.icm.uu.se/rosetta/) - The ROSETTA framework
* [R Core](https://www.R-project.org/) - R Core Team
