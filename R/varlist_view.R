#' List variables in the viewer, with some buttons to copy the table to clipboard, save the table as CSV/XLS/PDF, and print the table
#'
#' @param x a data.frame
#'
#' @return a data.frame
#' @importFrom DT datatable
#' @importFrom utils View
#' @import tibble
#' @import dplyr
#' @import lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' varlist.view(df) # df is a data frame
#' }
varlist.view <- function(x) {
  getlab <- function(x) attributes(x)[["label"]]
  label <- sapply(x, getlab)
  names <- colnames(x)
  varlist <- as.data.frame(cbind(names, label))
  varlist$label[varlist$label == "NULL"] <- NA
  options(scipen = 999)
  values <- lapply(x, function(x) if (is.factor(x)) {
    paste(levels(x), collapse = ", ")
  } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
    "Full NA"
  } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
    paste("TRUE, FALSE | ", round(sum(x, na.rm = T)/length(x) * 100), "% of TRUE", sep = "")
  } else if (is.character(x)) {dplyr::first(paste(substr(x, 1, 12),"..."))
  } else if (all(is.na(x))) {
    "Full NA"
  } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) | lubridate::is.POSIXt(x) | lubridate::is.Date(x)) {
    paste(min(x, na.rm = T), "...", max(x, na.rm = T))
  } else {
    paste(round(min(x, na.rm = T), digits = 4), "...", round(max(x, na.rm = T), digits = 4))
  })
  class <- lapply(x, function(x) if (lubridate::is.POSIXt(x) | lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x)) {
    paste(class(x), collapse = ", ")
  } else if (length(class(x)) > 1){
    paste(class(x), collapse = ", ")
  } else {
    class(x)
  })
  type <- sapply(x, typeof)
  varlist$values <- values
  varlist$class <- class
  varlist$type <- type
  varlist$valid <- apply(x, 2, function(x) length(x) - sum(is.na(x)))
  varlist$na <- apply(x, 2, function(x) sum(is.na(x)))
  varlist <- as.data.frame(lapply(varlist, unlist))
  varlist <- tibble::as_tibble(varlist)
  DT::datatable(varlist,
                editable = F,
                filter = 'none',
                caption = 'List of variables',
                selection = 'multiple',
                extensions = list("ColReorder" = NULL,
                                  "Buttons" = NULL,
                                  "KeyTable" = NULL,
                                  "FixedHeader" =NULL,
                                  "Select" = TRUE),
                options = list(dom = 'Blfrtip',
                               autoWidth=TRUE,
                               pageLength = 10,
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               colReorder = TRUE,
                               keys = TRUE,
                               searchHighlight = TRUE,
                               fixedHeader = TRUE
                               )
                )
}
