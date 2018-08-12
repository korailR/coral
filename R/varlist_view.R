#' List variables in the viewer
#'
#' @param d a data.frame
#'
#' @return a data.frame
#' @importFrom DT datatable
#' @importFrom utils View
#' @export
#'
#' @examples
#' \dontrun{
#' varlist.view(df)
#' }
varlist.view <- function(d) {
    myfunc.label <- function(x) attributes(x)[["label"]]
    variableslabel <- sapply(d, myfunc.label)
    Name <- names(variableslabel)
    variableslabel <- as.character(variableslabel)
    variableslabel[variableslabel == "NULL"] <- NA
    label <- (label = variableslabel)
    cl <- as.list(sapply(d, class))
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
    } else {
        paste(round(min(x, na.rm = T), digits = 4), "...", round(max(x, na.rm = T), digits = 4))
    })
    varlist <- data.frame(Name)
    varlist$Label <- label
    varlist$Values <- values
    varlist$Class <- cl
    varlist$Type <- type
    varlist$Valid <- valid
    varlist$NAs <- nas
    varlist <- as.data.frame(lapply(varlist, unlist))
    rownames(varlist) <- NULL
    DT::datatable(varlist, editable = F)
}

