#' Signed sqrt ggplot2 axis
#'
#' Wrapper for ggplot2::scale_x_sqrt
#' Also transforms the axis for negative values
#' To use native ggplot2 behavior, specify trans = "sqrt"
#' 
#' @param ... Passes to ggplot2::scale_x_sqrt
#' @return A ggplot2 axis
#' @export
#' @examples
#' ggplot(df) +
#'   geom_point(aes(x,y)) + 
#'   scale_x_sqrt()

scale_x_sqrt <- function(...) {
  S_sqrt = function(x) {
    sign(x) * sqrt(abs(x))
  }
  IS_sqrt = function(x) {
    x^2 * sign(x)
  }
  
  args <- list(...)
  if (is.null(args$trans)) {
    # If trans is not defined, use signed sqrt behavior
    args$trans = scales::trans_new(
      "signed_sqrt",
      S_sqrt,
      IS_sqrt
    )
  }
  
  return(do.call(ggplot2::scale_x_continuous, args))
}