#' Copy in the clipboard a data frame from View's RStudio to Excel
#'
#' @param x a data frame or matrix
#'
#' @examples
#' \dontrun{
#' copy_write_excel(x)
#' }

copy_write_excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-1000000",sep="\t",row.names=row.names,col.names=col.names,...)}
