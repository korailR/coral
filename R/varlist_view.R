#' List variables in the viewer, with some buttons to copy the table to clipboard, save the table as CSV/XLS/PDF, and print the table
#'
#' @param x a data.frame
#'
#' @return a data.frame
#' @importFrom DT datatable
#' @importFrom utils View
#' @import tibble
#' @import lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' varlist.view(df) # df is a data frame
#' }
varlist.view <- function(x) {
  Lab <- attributes(x)[["variable.labels"]]
  Names <- colnames(x)
  n <- length(x)
  Label <- if(is.null(Lab)) {NA
  } else {Lab}
  length(Label) <- n
  varlist <- cbind(Names, Label)
  varlist <- as.data.frame(varlist)
  varlist$Label[varlist$Label == "NULL"] <- NA
  options(scipen = 999)
  Values <- lapply(x, function(x) if (is.factor(x)) {
    paste(levels(x), collapse = ", ")
  } else if (is.logical(x) & (length(x) == sum(is.na(x)))) {
    "Full NA"
  } else if (is.logical(x) & (length(x) > sum(is.na(x)))) {
    paste(round(sum(x, na.rm = T)/n * 100), "% TRUE", sep = "")
  } else if (is.character(x)) {NA
  } else if (all(is.na(x))) {
    "Full NA"
  } else if (lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x) | lubridate::is.POSIXt(x) | lubridate::is.Date(x)) {
    paste(min(x, na.rm = T), "...", max(x, na.rm = T))
  } else {
    paste(round(min(x, na.rm = T), digits = 4), "...", round(max(x, na.rm = T), digits = 4))
  })
  Class <- lapply(x, function(x) if (lubridate::is.POSIXt(x) | lubridate::is.POSIXct(x) | lubridate::is.POSIXlt(x)) {
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
