#' Copy data frame from View to Excel
#'
#'
#' copy_write_excel(x)

copy_write_excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-1000000",sep="\t",row.names=row.names,col.names=col.names,...)}
