#' Map values
#'
#' Map values based on a from and to vector of equal length
#'
#' @param x Vector to find map values to
#' @param from Vector of values to match
#' @param to Vector of values to map to
#' @param strict Logical. Strict type checking
#' @return Vector with values mapped
#' @export
#' @examples
#' mapvalues(c(1,2,3), c(3,2,1), c(6,5,4))

mapvalues <- function(x, from, to, strict = F) {
  # Condition check
  if (length(to) != length(from)) {
    warning("Arguments to and from should be of same length!")
    return()
  }
  if (TRUE %in% duplicated(from)){
    warning("Vector with 'from' values cannot contain duplicates")
    return()
  }
  
  if (strict & 
      (typeof(x) != typeof(from) | typeof(x) != typeof(from))) {
    warning("When using strict type-matching, vector types must match!")
    return()
  }
  # Type for return and coerce to str
  type_in = typeof(x)
  type_to = typeof(to)
  
  x = as.character(x)
  from = as.character(from)
  to = as.character(to)
  
  # Run
  x = rcpp_mapvalues(x, from, to)
  
  # Set types before return if 
  output = x
  numerics = c("double", "integer", "numeric")
  if (type_to == "logical") output = as.logical(output)
  if (type_to  %in% numerics) output = as.numeric(output)
  
  if (TRUE %in% is.na(output)) output = x
  if (type_in == "logical") output = as.logical(output)
  if (type_in  %in% numerics) output = as.numeric(output)
  
  if (TRUE %in% is.na(output)) output = x
  return(x)
}
