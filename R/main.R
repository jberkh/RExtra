#Easier string concatenations
"+" = function(x,y) {
    if (is.character(x) || is.character(y)) {
        return(paste0(x , y))
    } else {
        .Primitive("+")(x,y)
    }
}

#String multiplication
"*" = function(x,y) {
    if (is.character(x) & is.numeric(y)) {
        return(paste0(rep(x,y), collapse = ""))
    } else if (is.character(y) & is.numeric(x)) {
        return(paste0(rep(y, x), collapse = ""))
    } else {
	.Primitive("*")(x,y)
    }
}

# Rep() operator
"%x%" = function(A,B) return(rep(A,B))


#Extra %in% functionality
'%!in%' <- function(A,B) return(! A %in% B)

#Import csv with vroom() but with rownames
vroom <- function(file, delim = NULL, row_names = FALSE, ...){
  df <- vroom::vroom(file, delim, ...)
  if (row_names) {
    df <- as.data.frame(df)
    row.names(df) <- df[,1]
    df <- df[,-1]
  }
  return(df)
}

#Mapvalues w/ dplyr
mapvalues <- function(x, from, to) {
	return(dplyr::recode(x, !!!setNames(to, from)))
}

#Better duplicated()
duplicated <- function(x, incomparables = FALSE, fromLast = FALSE, nmax = NA, allItems = FALSE, ...) {
	if (allItems) {
		return(base::duplicated(x, incomparables = incomparables, fromLast = FALSE, nmax = NA, ...) | base::duplicated(x, incomparables = incomparables, fromLast = TRUE, nmax = NA, ...))
	} else {
		return(base::duplicated(x, incomparables = incomparables, fromLast = fromLast, nmax = NA, ...))
	}
}
