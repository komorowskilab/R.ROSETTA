# RROSETTA

**ROSETTA** rough set classifier as a R package. In addition to all the existing ROSETTA (created by Öhrn and Komorowski in 1997) algorithms we have added new functions especially useful in bioinformatics applications. 
These include: 
* under-sampling
* p-value estimation of the rules
* clustering decision classes
* retrieving the support sets for rules
* reclassification

Package contains following functions:<br />
**rosetta()** <-> *runs ROSETTA rough set classifier*<br />
**recalculateRules()** <-> *recaluclates rules after undersampling and retrieves the support sets for rules*<br />
**saveLineByLine()** <-> *saves rules to Line By Line format, compatibile with VisuNet http://bioinf.icm.uu.se/~visunet/*<br />
**getDecision()** <-> *reclassifies model using created rules*<br />

**Installation guide:**<br />
install.packages("devtools")<br /><br />
library(devtools)<br />
install_github("mategarb/RROSETTA")
