#' List variables
#'
#' @param x a data.frame
#'
#' @return a data.frame
#' @import tidyverse
#' @import lubridate
#' @export
#'
#' @examples#' \dontrun{
#' varlist(df) # df is a data frame
#' varlist(df, tdf = TRUE) # if tdf = TRUE, print a tibble data format 
#' }
varlist <- function(x, tdf = FALSE) {
  getlab <- function(x) attributes(x)[["label"]]
  Label <- sapply(x, getlab)
  Names <- colnames(x)
  varlist <- as.data.frame(cbind(Names, Label))
  varlist$Label[varlist$Label == "NULL"] <- NA
  options(scipen = 999)
  Values <- lapply(x, function(x) if (is.factor(x)) {
    paste(levels(x), collapse = ", ")
    } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
      "Full NA"
      } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
        paste(round(sum(x, na.rm = T)/n * 100), "% TRUE", sep = "")
        } else if (is.character(x)) {first(paste(substr(x, 1, 12),"..."))
          } else if (all(is.na(x))) {
            "Full NA"
            } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) | lubridate::is.POSIXt(x) | lubridate::is.Date(x)) {
              paste(min(x, na.rm = T), "...", max(x, na.rm = T))
              } else {
                paste(round(min(x, na.rm = T), digits = 4), "...", round(max(x, na.rm = T), digits = 4))
                })
  Class <- lapply(x, function(x) if (lubridate::is.POSIXt(x) | lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x)) {
    paste(class(x), collapse = ", ")
  } else if (length(class(x)) > 1){
    paste(class(x), collapse = ", ")
  } else {
    class(x)
  })
  Type <- sapply(x, typeof)
  varlist$Values <- Values
  varlist$Class <- Class
  varlist$Type <- Type
  varlist$Valid <- apply(x, 2, function(x) length(x) - sum(is.na(x)))
  varlist$NAs <- apply(x, 2, function(x) sum(is.na(x)))
  varlist <- as.data.frame(lapply(varlist, unlist))
  varlist <- tibble::as_tibble(varlist)
  ifelse(tdf, return(varlist), return(View(varlist, paste("varlist", deparse(substitute(x)), sep = " "))))
  }
