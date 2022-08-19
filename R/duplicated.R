#' Duplicated() wrapper
#'
#' Adds functionality to base duplicated function to return all duplicated items
#'
#' @param x Vector to find duplicates in
#' @param incomparables Same as in base::duplicated()
#' @param fromLast Same as in base::duplicated(). Ignored when allItems is set TRUE
#' @param nmax Same as in base::duplicated()
#' @param allItems Logical. Return all duplicated items
#' @return A multiplicated string
#' @export
#' @examples
#' duplicated(c(1,1,2,3,4,5,5), allItems = T)

duplicated <- function(x, incomparables = FALSE, fromLast = FALSE, nmax = NA, allItems = FALSE, ...) {
  if (allItems) {
    return(base::duplicated(x, incomparables = incomparables, fromLast = FALSE, nmax = NA, ...) | base::duplicated(x, incomparables = incomparables, fromLast = TRUE, nmax = NA, ...))
  } else {
    return(base::duplicated(x, incomparables = incomparables, fromLast = fromLast, nmax = NA, ...))
  }
}