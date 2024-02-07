#' Log-transformed ggplot2 axis
#'
#' Wrapper for ggplot2::scale_x_log10.
#' With correct log minor breaks
#' 
#' @param ... Passes to ggplot2::scale_x_log10
#' @return A ggplot2 axis
#' @export
#' @examples
#' ggplot(df) +
#'   geom_point(aes(x,y)) + 
#'   scale_x_log10()

scale_x_log10 <- function(...) {
  # Minor break function
  log10_minor_break = function (...){
    function(x) {
      minx         = floor(min(log10(x), na.rm=T))-1;
      maxx         = ceiling(max(log10(x), na.rm=T))+1;
      n_major      = maxx-minx+1;
      major_breaks = seq(minx, maxx, by=1)
      minor_breaks = 
        rep(log10(seq(1, 9, by=1)), times = n_major)+
        rep(major_breaks, each = 9)
      return(10^(minor_breaks))
    }
  }
  
  args <- list(...)  # Capture all arguments in a list
  if (!"minor_breaks" %in% names(args)) {
    # If not defined, use the log10 minor break function
    args$minor_breaks <- log10_minor_break()
  }
  return(do.call(ggplot2::scale_x_log10, args))
}