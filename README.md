**Note**: This package has been maintained by [@terrytangyuan](https://github.com/terrytangyuan) since 2019. Please [consider sponsoring](https://github.com/sponsors/terrytangyuan)!
# scaffolder

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/scaffolder)](https://cran.r-project.org/package=scaffolder)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The **scaffolder** package provides a comprehensive set of tools to automate the process of scaffolding interfaces to modules, classes, functions, and documentations written in other programming languages. As initial proof of concept, scaffolding R interfaces to Python packages is supported via [reticulate](https://github.com/rstudio/reticulate).

### Why do we need this package?

Let's say if we want to write an interface to an existing Python package. **reticulate** package is the perfect tool to facilitate that. **reticulate** package contains a powerful set of tools for interoperability between Python and R. It allows R users to call Python from R in many different ways and automatically translate between different R and Python objects.

However, if we use **reticulate** often enough, we will face many challenges, for example:

* We need to copy-paste doc-strings from Python API and then edit it in order to provide API references for the R wrapper functions. This requires a lot of editing and endless maintenance efforts in the future to keep the consistency between the API references in both Python and R.
* We need to copy-paste the default values for all the parameters and translate Python objects to R objects when writing the R wrapper functions (e.g. `None` -> `NULL`, `True` -> `TRUE`, convert Python list literals in the docs to R lists, massage R numeric values to Python integers via `as.integer` where required, etc.). And then again, this requires future maintenance in order to keep them up-to-date.

The **scaffolder** package comes to the rescue. It provides functions to automatically scaffold and customize the R wrapper interfaces to modules, classes, functions, and documentations written in other programming languages. Users are able to customize different parts of the scaffolding process in order to avoid the additional editing that we mentioned above. For example, users can implement a function to cast parameters with default values that contains "L" to integers.

### History

The core functionalties of this package were originally developed as part of **reticulate**'s [scaffolding functionalities](https://github.com/rstudio/reticulate/blob/a046bd12eecc7c879ff14637367d661c707b5d8d/R/wrapper.R) to facilitate the process of writing different components of [the R interface to TensorFlow](https://tensorflow.rstudio.com/). We found the process of writing R wrappers very tedious and then developed the scaffolding functionalities in **reticulate** that are marked for internal use only. It was then further developed and experimented to automate the generation of [h2oai/h2o4gpu's R package](https://github.com/h2oai/h2o4gpu/tree/2af2bc75cef4ad93a28b80c1359c2cefe758a643/src/interface_r/).

### Getting started

#### Installation

Install the **scaffolder** package from CRAN as follows:
```r
install.packages("scaffolder")
```

Or you can install it from GitHub as follows:

```r
install.packages("remotes")
remotes::install_github("terrytangyuan/scaffolder")
```

#### Examples

Please checkout the examples [here](https://terrytangyuan.github.io/scaffolder/articles/intro.html).
