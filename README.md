# datamgmt
### *Data management utilities for curating, documenting, and publishing data*

[![Build Status](https://travis-ci.org/NCEAS/datamgmt.svg?branch=master)](https://travis-ci.org/NCEAS/datamgmt)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/datamgmt)](https://cran.r-project.org/package=datamgmt)

- **Authors**: Data Science Fellows, Spring 2018
- **License**: [Apache 2](http://opensource.org/licenses/Apache-2.0)
- [Package source code on Github](https://github.com/NCEAS/datamgmt)
- [**Submit Bugs and feature requests**](https://github.com/NCEAS/datamgmt/issues)

The *datamgmt* R package supports management of data packages on the [Arctic Data Center](https://arcticdata.io/) (ADC) and [State of Alaska's Salmon and People](https://alaskasalmonandpeople.org/) (SASAP) data portals. 

## Installation
You can install the development version from github with:

```{r}
# install.packages("devtools")

devtools::install_github("NCEAS/datamgmt")
```

## Style
We generally follow the [tidyverse style conventions](http://style.tidyverse.org/), with the following specific style preferences: 

- underscore for all variable names unless referring to an EML object (i.e. otherEntity, publicationDate, etc.)
- all functions should include argument checks in the form of `stopifnot` statements

## Acknowledgements
Work on this package was supported by:

- The Arctic Data Center: NSF-PLR grant #1546024 to M. B. Jones, S. Baker-Yeboah, J. Dozier, M. Schildhauer, and A. Budden

Additional support was provided by the National Center for Ecological Analysis and Synthesis, a Center funded by the University of California, Santa Barbara, and the State of California.

[![nceas_footer](https://www.nceas.ucsb.edu/files/newLogo_0.png)](http://www.nceas.ucsb.edu)
