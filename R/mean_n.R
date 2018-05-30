#' Computing mean over variables and dealing with NAs
#'
#' @param vars variables (columns) of a data.frame
#' @param not.na the minimum number of valid answers (not NA)
#'
#' @return mean
#' @export
#'
#' @examples
#' \dontrun{
#' mean_n(df[2:5,3])
#' }
mean_n <- function(vars, not.na = 0) {
    apply(vars, 1, function(x) ifelse(sum(!is.na(x)) >= not.na, mean(x, na.rm = TRUE),
        NA))
}
