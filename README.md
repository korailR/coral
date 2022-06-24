coral
=========

## Overview

coral contains a set of functions for:
- Listing variables of a data frame in three ways
- Computing row means from a `data.frame` or `matrix` and dealing with NAs

Functions `varlist()`, `varlist_shiny()` and `varlist_view()` contain descriptions of the attributes of each variable in the data frame. Rows are variables and columns are variable attributes (label, values, class, typeof, number of valid rows and NAs).

  - `varlist(x, values = c("min_max", "all"), to_df = FALSE)` creates a data frame of variables in source editor of RStudio
  - `varlist_shiny(x, values = c("min_max", "all"), to_df = FALSE)` creates an HTML table widget to display data frame of variables
  - `varlist_view(x, values = c("min_max", "all"), to_df = FALSE)` creates a data frame of variables in viewer pane


Function `mean_n(x, n)` computes row means from a `data.frame` or `matrix`. The first argument of `mean_n()` is variables (columns of a data frame or matrix) and the second argument a numeric value indicates the minimum amount of valid values (not NA) per row to calculate row mean (`default = 1`). Numeric value between 0 and 1 indicates a proportion of valid values per row to calculate the row mean.

Function `sum_n(x, n)` computes row sums from a `data.frame` or `matrix`. The first argument of `sum_n()` is variables (columns of a data frame or matrix) and the second argument a numeric value indicates the minimum amount of valid values (not NA) per row to calculate row sum (`default = 1`). Numeric value between 0 and 1 indicates a proportion of valid values per row to calculate the row sum.


### Usage  

`varlist(df, values = c("min_max", "all"), to_df = FALSE)`          # df is a data frame; if to_df = TRUE, print a tibble data format (default = FALSE)
                                                                    # if values = "min_max" (default), display minimum and maximum values of columns
                                                                    # if values = "all", display all values of columns

`varlist_shiny(df,values = c("min_max", "all"))`     # df is a data frame  

`varlist_view(df, values = c("min_max", "all"))`     # df is a data frame  

`mean_n(mat)`              #  compute row means over mat (a matrix) if there is at least one valid value (default value of n = 1)

`mean_n(df[,3:7], 3)`      #  compute row means over columns 3 to 7 of df (a data frame) if the minimum amount of valid values (not NA) is 3 per row

`mean_n(df[,3:7], 0.8)`    #  compute mean over columns 3 to 7 of df (a data frame) if the proportion of valid values (not NA) is 0.8 per row

`sum_n(mat)`              #  compute row sums over mat (a matrix) if there is at least one valid value (default value of n = 1)

`sum_n(df[,3:7], 3)`      #  compute row sums over columns 3 to 7 of df (a data frame) if the minimum amount of valid values (not NA) is 3 per row

`sum_n(df[,3:7], 0.8)`    #  compute sum over columns 3 to 7 of df (a data frame) if the proportion of valid values (not NA) is 0.8 per row

## Installation


Latest version from GitHub :



```r

library(devtools)

install_github("korailR/coral")

# Alternative
library(remotes)

install_github("korailR/coral")


```



## Documentation

No
