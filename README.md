[![Travis-CI Build Status](https://travis-ci.org/aquaMetrics/rict.svg?branch=master)](https://travis-ci.org/aquaMetrics/rict)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/aquaMetrics/rict?branch=master&svg=true)](https://ci.appveyor.com/project/ecodata1/rict/branch/master)
[![codecov](https://codecov.io/gh/aquaMetrics/rict/branch/master/graph/badge.svg)](https://codecov.io/gh/aquaMetrics/rict)


# Work in progress

This R package is in development. Please don't use in production. Test 

## rict package

An R package for calculating River Invertebrate Classification Tool (rict) predictions

In R: Install the development version from GitHub:
```
install.packages("devtools")
library(devtools)
install_github("aquaMetrics/rict")
```

Run the demo dataset through the `calcPrediction` function to get predicted scores:

```
predictions <- calcPrediction(observed_values = rict::demo_observed_values)
```

Then run the predictions through `calcClassification` to compare the predicted scores against your observed scores, giving you the classification output:

```
classifications <- calcClassification(predictions, year_type = "multi")
```

## Contributing 

Please read the [Contributing guidelines](CONTRIBUTING.md) file for more details 


## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.