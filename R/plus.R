#' String concatenation
#'
#' Concatenate strings with the + operator
#'
#' @param x String object or string vector
#' @param y String object string vector. Coerced to string if other type
#' @return A concatenated string (vector)
#' @export
#' @examples
#' "Hello " + "World"
#' "Hello " + c("World", "Moon")
#' "Label_" + 1:5

"+" = function(x,y) {
  contains_str = 
    is.character(x) || 
    is.character(y)
  len_condition = 
    length(x) == 1 || 
    length(y) == 1 ||
    length(x) == length(y)
  if (contains_str && len_condition) {
    # Str concat in specific conditions
    return(paste0(x , y))
  } else {
    # Base R behavior
    .Primitive("+")(x,y)
  }
}
