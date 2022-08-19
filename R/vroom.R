#' Vroom wrapper with row_name functionality
#'
#' Use vroom::vroom() with added functionality for row_names
#'
#' @param file File to open
#' @param delim Delimiter passed to vroom
#' @param row_names Logical. File has row.names (Default = FALSE)
#' @return A data.frame
#' @export
#' @examples
#' data <- vroom("file.csv", row_names = T)

vroom <- function(file, delim = NULL, row_names = FALSE, ...){
  df <- vroom::vroom(file, delim, ...)
  if (row_names) {
    df <- as.data.frame(df)
    row.names(df) <- df[,1]
    df <- df[,-1]
  }
  return(as.data.frame(df))
}