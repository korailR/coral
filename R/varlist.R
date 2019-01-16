#' List variables
#'
#' @param d a data.frame
#'
#' @return a data.frame
#' @export
#'
#' @examples
varlist <- function(d) {
  myfunc.label <- function(x) attributes(x)[["label"]]
  variableslabel <- sapply(d, myfunc.label)
  name <- names(variableslabel)
  variableslabel <- as.character(variableslabel)
  variableslabel[variableslabel == "NULL"] <- NA
  label <- (label = variableslabel)
  class <- as.list(sapply(d, class))
  type <- as.list(sapply(d, typeof))
  valid <- as.list(sapply(d, function(x) length(x) - sum(is.na(x))))
  nas <- as.list(sapply(d, function(x) sum(is.na(x))))
  options(scipen = 999)
  values <- lapply(d, function(x) if (is.factor(x)) {
    paste(levels(x), collapse = ", ")
  } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
    "Full NA"
  } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
    paste(round(sum(x, na.rm = T)/n * 100), "% TRUE", sep = "")
  } else if (is.character(x)) {
    NA
  } else if (all(is.na(x))) {
    "Full NA"
  } else if (is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x) | is.Date(x)) {
    paste(min(x, na.rm = T), "...", max(x, na.rm = T))
  } else {
    paste(round(min(x, na.rm = T), digits = 4), "...", round(max(x, na.rm = T), digits = 4))
  })
  varlist <- data.frame(name)
  varlist$Label <- label
  varlist$Values <- values
  varlist$Class <- lapply(d, function(x) ifelse(is.POSIXt(x) | is.POSIXct(x) | is.POSIXlt(x),
                                                paste(class(d$startdate), collapse = ", "), class))
  varlist$Type <- type
  varlist$Valid <- valid
  varlist$NAs <- nas
  varlist <- as.data.frame(lapply(varlist, unlist))
  View(varlist, paste("varlist", deparse(substitute(d)), sep = " "))
}
