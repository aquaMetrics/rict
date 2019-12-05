---
output:
  html_document: default
  pdf_document: default
---
[![Travis-CI Build Status](https://travis-ci.org/aquaMetrics/rict.svg?branch=master)](https://travis-ci.org/aquaMetrics/rict)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/aquaMetrics/rict?branch=master&svg=true)](https://ci.appveyor.com/project/ecodata1/rict/branch/master)
[![codecov](https://codecov.io/gh/aquaMetrics/rict/branch/master/graph/badge.svg)](https://codecov.io/gh/aquaMetrics/rict)

# Work in progress

This R package is in development. Please don't use in production.

## rict package

An R package for calculating River Invertebrate Classification Tool (rict) predictions

## Install

Install [R](rstats.org/install) and then recommend installing [Rstudio](rstudio.com/install).

Open Rstudio.
  
In R: Install the development version from GitHub:
```
install.packages("devtools")
library(devtools)
install_github("aquaMetrics/rict")
library(rict)
```
Run the demo dataset through the `rict_predict` function to get predicted scores:

```
predictions <- rict_predict(demo_observed_values)
```
Then run the predictions through `rict_classify` to compare the predicted scores against your observed scores, giving you the classification output:

```
class_results <- rict_classify(predictions)
```
The `demo_observed_values` contain physical environmental variables such width and depth. By default, the above code runs a multi-year prediction based on the Great Britain model. These default can be overridden:

```
predictions <- rict_predict(demo_observed_values, year_type = "single", area="ni")
```
This code runs a single year output using validation checks for Northern Ireland. See documentation for more information.

## Contributing 

Please read the [Contributing guidelines](CONTRIBUTING.md) file for more details 

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.