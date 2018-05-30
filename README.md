coral
=========

## Overview

coral contains a set of functions for:
- Listing variables of a data frame in three ways
- Computing mean over variables and dealing with NAs


Functions `varlist()`, `varlist.shiny()` and `varlist.view()` contain descriptions of the attributes of each variable in the data frame. Rows are variables and columns are variable attributes (label, values, class, typeof, number of valid rows and NAs).

  - `varlist()` creates a data frame of variables in source editor of RStudio
  - `varlist.shiny()` creates an HTML table widget to display data frame of variables
  - `varlist.view()` creates a data frame of variables in viewer pane


Function `mean_n()` computes means over variables. The first argument of `mean_n()` is variables (in columns) and the second argument is the minimal number of variables  that should be non-missing (not NA)

### Usage  

`varlist(df)`  #  df is a data frame


`mean_n(df[3:7],3)`  #  compute mean over columns 3 to 7 of df (a data frame) and it takes at least 3 valid answers (no NA)



## Installation


Latest version from GitHub :



```r

library(devtools)

install_github("korailR/coral")

```



## Documentation

No
