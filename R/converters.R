#' Convert R vector/matrix to yacas vector (list) or matrix (list of lists)
#' 
#' @param x R vector to convert
#' 
#' @concept helper
#' 
#' @export
as_y <- function(x) {
  if (is.matrix(x)) {
    return(y_hlp_to_yacmat(x))
  }
  
  return(y_hlp_to_yacvec(x))
}

#' Convert yacas vector (list) or matrix (list of lists) to R vector/matrix
#' 
#' @param x yacas list or list of lists to convert
#' 
#' @concept helper
#' 
#' @export
as_r <- function(x) {
  # Two {'s in line, potentially with space in between
  if (grepl("\\{[ ]*\\{", x)) {
    # Matrix
    return(y_hlp_from_yacmat(x))
  }
  
  # Vector
  return(y_hlp_from_yacvec(x))
}

