#' Not in operator
#'
#' Shows elements not in other vector
#'
#' @param x Vector 1
#' @param y Vector 2
#' @return Logical vector
#' @name %!in%
#' @usage x \%!in\% y
#' @export
#' @examples
#' c(1,2,3) %!in% c(1,3)
#' @rdname nin

'%!in%' <- function(x,y) return(! x %in% y)