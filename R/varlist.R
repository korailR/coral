#' List variables
#'
#' @param d a data.frame
#'
#' @return a data.frame
#' @export
#'
#' @examples#' \dontrun{
#' varlist(df) # df is a data frame
#' varlist(df, tdf = TRUE) # if tdf = TRUE, print a tibble data format 
#' }
varlist <- function(d, tdf = FALSE) {
  myfunc.label <- function(x) attributes(x)[["label"]]
  variableslabel <- sapply(d, myfunc.label)
  Names <- names(variableslabel)
  varlist <- as.data.frame(Names)
  varlist$Label <- as.character(variableslabel)
  varlist$Label[varlist$Label == "NULL"] <- NA
  options(scipen = 999)
  Values <- lapply(d, function(x) if (is.factor(x)) {
    paste(levels(x), collapse = ", ")
    } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
      "Full NA"
      } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
        paste(round(sum(x, na.rm = T)/n * 100), "% TRUE", sep = "")
        } else if (is.character(x)) {NA
          } else if (all(is.na(x))) {
            "Full NA"
            } else if (is.POSIXct(x) | is.POSIXlt(x) | is.POSIXt(x) | is.Date(x)) {
              paste(min(x, na.rm = T), "...", max(x, na.rm = T))
              } else {
                paste(round(min(x, na.rm = T), digits = 4), "...", round(max(x, na.rm = T), digits = 4))
                })
  Class <- lapply(d, function(x) if (is.POSIXt(x) | is.POSIXct(x) | is.POSIXlt(x)) {
    paste(class(x), collapse = ", ")
  } else {
    class(x)
  })
  Type <- sapply(d, typeof)
  
  varlist$Values <- Values
  varlist$Class <- Class
  varlist$Type <- Type
  varlist$Valid <- apply(d, 2, function(x) length(x) - sum(is.na(x)))
  varlist$NAs <- apply(d, 2, function(x) sum(is.na(x)))
  varlist <- as.data.frame(lapply(varlist, unlist))
  varlist <- as_tibble(varlist)
  
  ifelse(tdf ,return(varlist), return(View(varlist, paste("varlist", deparse(substitute(d)), sep = " "))))
  }
