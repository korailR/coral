#' List variables
#'
#' @param x a data.frame
#'
#' @return a data.frame
#' @import tibble
#' @import dplyr
#' @import lubridate
#' @export
#'
#' @examples#' \dontrun{
#' varlist(df) # df is a data frame
#' varlist(df, values = c("min_max", "all"), to_df = FALSE) # if values = "min_max" (default), display minimum and maximum values of columns, if values = "all", display all values of columns
#' varlist(df, values = c("min_max", "all"), to_df = FALSE) # if to_df = TRUE, print a tibble data format 
#' }

varlist <- function(x, values = c("min_max", "all"), to_df = FALSE) {
  getlab <- function(x) attributes(x)[["label"]]
  label <- sapply(x, getlab)
  names <- colnames(x)
  varlist <- as.data.frame(cbind(names, label))
  varlist$label[varlist$label == "NULL"] <- NA
  options(scipen = 999)
  min_max <- lapply(x, function(x) {
    if (is.factor(x)) {
      paste(levels(x), collapse = ", ")
    } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
      "Full NA"
    } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
      paste(sort(unique(na.omit(x))), collapse = ", ")
    } else if (is.character(x)) {
      paste(dplyr::first(na.omit(substr(x, 1, 20))), "...", dplyr::last(na.omit(substr(x, 1, 20))))
    } else if (all(is.na(x))) {
      "Full NA"
    } else if (is.list(x)) {
      "Lists"
    } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) |
      lubridate::is.POSIXt(x) | lubridate::is.Date(x)) {
      paste(min(x, na.rm = T), "...", max(x, na.rm = T))
    } else if (n_distinct(x, na.rm = T) < 6) {
      paste(sort(unique(na.omit(x))), collapse = ", ")
    } else {
      paste(paste(nth(sort(unique(na.omit(x))), 1),
        nth(sort(unique(na.omit(x))), 2),
        nth(sort(unique(na.omit(x))), 3),
        nth(sort(unique(na.omit(x))), 4),
        nth(sort(unique(na.omit(x))), 5),
        sep = ", "
      ), " ... ", max(x, na.rm = T), sep = "")
    }
  })

  all <- lapply(x, function(x) {
    if (is.factor(x)) {
      paste(levels(x), collapse = ", ")
    } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
      "Full NA"
    } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
      paste(sort(unique(na.omit(x))), collapse = ", ")
    } else if (is.character(x)) {
      paste(sort(unique(na.omit(x))), collapse = ", ")
    } else if (all(is.na(x))) {
      "Full NA"
    } else if (is.list(x)) {
      "Lists"
    } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) |
      lubridate::is.POSIXt(x) | lubridate::is.Date(x)) {
      paste(sort(unique(na.omit(x))), collapse = ", ")
    } else {
      paste(sort(unique(na.omit(x))), collapse = ", ")
    }
  })

  class <- lapply(x, function(x) {
    if (lubridate::is.POSIXt(x) |
      lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x)) {
      paste(class(x), collapse = ", ")
    } else if (length(class(x)) > 1) {
      paste(class(x), collapse = ", ")
    } else {
      class(x)
    }
  })

  values <- match.arg(values)

  type <- sapply(x, typeof)
  varlist$values <- switch(values,
    min_max = min_max,
    all = all
  )
  varlist$class <- class
  varlist$type <- type
  varlist$valid <- apply(x, 2, function(x) length(x) - sum(is.na(x)))
  varlist$na <- apply(x, 2, function(x) sum(is.na(x)))
  varlist <- as.data.frame(lapply(varlist, unlist))
  varlist <- tibble::as_tibble(varlist)
  ifelse(to_df, return(varlist), return(View(varlist, paste("varlist",
    deparse(substitute(x)),
    sep = " "
  ))))
}
