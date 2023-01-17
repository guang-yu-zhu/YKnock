# YKnock <img src="https://github.com/guang-yu-zhu/YKnock/raw/master/img/logo.png" align="right" width=15% />

  [![R build status](https://github.com/microsoft/wpa/workflows/R-CMD-check/badge.svg)](https://github.com/guang-yu-zhu/YKnock)
  [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT/)
  [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
  [![CRAN status](https://www.r-pkg.org/badges/version/wpa)](https://CRAN.R-project.org/)

## Installation
```
devtools::install_github('guang-yu-zhu/Yknock')
```

## Package

This is an R package for perform Controlled Response Variable Selection.

Large-scale multiple perturbation experiments have the potential to reveal a more detailed understanding of the molecular pathways that respond to genetic and environmental changes. A key question in these studies is which gene expression changes are important for the response to the perturbation. This problem is challenging because

* the functional form of the nonlinear relationship between gene expression and the perturbation is unknown, 
* identification of the most important genes is a high-dimensional variable selection problem. 

To deal with these challenges, we present here a method based on the model-X knockoffs framework and Deep Neural Networks (DNNs) to identify significant gene expression changes in multiple perturbation experiments. This approach makes no assumptions on the functional form of the  dependence between the responses and the perturbations and it enjoys finite sample false discovery rate control for the selected set of important gene expression responses. 


We apply this approach to the Library of Integrated Network-Based Cellular Signature (LINCS) data sets which is an NIH Common Fund program that catalogs how human cells globally respond to chemical, genetic and disease perturbations. We identified important genes whose expression is directly modulated in response to perturbation with anthracycline, vorinostat, trichostatin-a, geldanamycin, and sirolimus. We compare the set of important genes that respond to these small molecules to identify co-responsive pathways. Identification of which genes respond to specific perturbation stressors can provide better understanding of  the underlying mechanisms of disease and advance the identification of new drug targets. 

