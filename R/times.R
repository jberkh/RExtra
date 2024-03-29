#' Non-numeric multiplication
#'
#' Multiplicate non-numerics with the * operator
#'
#' @param x Non-numeric object
#' @param y Numeric
#' @return A multiplicated string
#' @export
#' @examples
#' "Hello World!" * 2

"*" = function(x,y) {
  if (
    !is.numeric(x) & 
    !is.logical(x) &
    !isS4(x) & 
    is.numeric(y)
  ) {
    return(rep(x,y))
    
  } else if (!is.numeric(y) & is.numeric(x)) {
    # Recursively handle x, y order reversed
    return(y * x)

  } else {
    .Primitive("*")(x,y)
  }
}
