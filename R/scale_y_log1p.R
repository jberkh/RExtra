#' (Signed) Log1p ggplot2 axis
#'
#' Creates an axis using (signed) log1p as transformation
#' Also transforms the axis for negative values
#' 
#' @param ... Passes to ggplot2::scale_y_continuous
#' @return A ggplot2 axis
#' @export
#' @examples
#' ggplot(df) +
#'   geom_point(aes(x,y)) + 
#'   scale_y_log1p()

scale_y_log1p <- function(...) {
  S_log1p = function(x) {sign(x) * log1p(abs(x))}
  IS_log1p = function(x) {(exp(abs(x)) - 1) * sign(x)}
  
  # Use 2 as nearest intuitive base number
  log1p_breaks = function (...){
    function(x) {
      minx         = floor(min(S_log1p(x), na.rm = T)) * 2;
      maxx         = ceiling(max(S_log1p(x), na.rm = T)) * 2;
      n_major      = maxx - minx + 1;
      
      IS_log2 = function(x) {2**abs(x) * sign(x)}
      major_breaks = seq(minx, maxx, by = 1)
      major_breaks = c(-1:1, IS_log2(major_breaks))
      return(sort(major_breaks))
    }
  }
  
  args <- list(...)
  if (!"trans" %in% names(args)) {
    # If trans is not defined, use signed sqrt behavior
    args$trans = scales::trans_new(
      "signed_log1p",
      S_log1p,
      IS_log1p
    )
  }
  if (!"breaks" %in% names(args)) {
    args$breaks = log1p_breaks()
  }
  
  return(do.call(ggplot2::scale_y_continuous, args))
}