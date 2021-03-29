<!--
  Title: R.ROSETTA
  Description: ROSETTA rough set classifier as a R package.
  Author: Mateusz Garbulowski
  -->
<meta name='keywords' content='rosetta, rough sets, classification, transparent machine learning'>

# R.ROSETTA
R.ROSETTA is an R package for constructing and analyzing rule-based classification models. R.ROSETTA is designed to support the overall data mining and knowledge discovery process. The presented tool is a more accessible and extended version of ROSETTA system (Öhrn and Komorowski, 1997). In addition to all the existing ROSETTA functionalities, we have added new functions such as: balancing data with undersampling, estimating rule p-value, retrieving support sets from rules, predicting external classes, visualizing rule-based model and generating synthetic data. 

For more information and tutorials, please visit the [official R.ROSETTA website](https://komorowskilab.github.io/R.ROSETTA/).


### Prerequisites
R.ROSETTA works with UNIX and Windows OS. However, UNIX operating systems (like MAC or Linux) require 32-bit [Wine](https://www.winehq.org/) - a free and open-source compatibility layer. Please notice that latest version of macOS (Catalina) no longer supports 32-bit apps. Thus, we suggest to use [VirtualBox](https://www.virtualbox.org/) or [Docker](https://www.docker.com/why-docker).

### Installation

Installation from github requires devtools package:
```R
install.packages("devtools")
```

Installation and loading R.ROSETTA package from github:
```R
library(devtools)
install_github("komorowskilab/R.ROSETTA")

library(R.ROSETTA)
```

## Acknowledgments
R.ROSETTA includes a sample dataset collected from GEO repository with the reference number [GSE25507](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE25507).

### Built With
* [ROSETTA](http://bioinf.icm.uu.se/rosetta/) - The ROSETTA framework
* [R Core](https://www.R-project.org/) - R Core Team

More information about ROSETTA can be found at [ROSETTA technical reference manual](http://bioinf.icm.uu.se/rosetta/materials/manual.pdf)
## Related work
- VisuNet: [An interactive tool for network visualization of rule-based models in R](https://github.com/komorowskilab/VisuNet)

## Citation
Garbulowski M, Diamanti K, Smolińska K, Baltzer N, Stoll P, Bornelöv S, Øhrn A, Feuk L, Komorowski J. R.ROSETTA: an interpretable machine learning framework. BMC Bioinformatics. 2021 Mar 6;22(1):110. doi: 10.1186/s12859-021-04049-z
[link](https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-021-04049-z)
