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
  S_sqrt = function(x) {sign(x) * sqrt(abs(x))}
  IS_sqrt = function(x) {x^2 * sign(x)}
  
  sqrt_minor_breaks = function (...){
    function(x) {
      minx         = floor(min(x, na.rm = T)) - 1
      maxx         = ceiling(max(x, na.rm = T)) + 1
      limits       = c(minx, maxx)
      major_breaks = scales::extended_breaks()(x)
      minor_breaks = scales::regular_minor_breaks()(
        major_breaks, limits, 2
      )
      return(minor_breaks)
    }
  }
  
  args <- list(...)
  if (!"trans" %in% names(args)) {
    # If trans is not defined, use signed sqrt behavior
    args$trans = scales::trans_new(
      "signed_sqrt",
      S_sqrt,
      IS_sqrt
    )
  }
  if (!"minor_breaks" %in% names(args)) {
    # If minor breaks not defined, use sqrt appropriate minbreaks
    args$minor_breaks = sqrt_minor_breaks()
  }
  
  return(do.call(ggplot2::scale_x_continuous, args))
}