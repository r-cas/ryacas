#' Get Yacas variables
#' 
#' @export
ls_yacas <- function() {
  x <- yacas("Variables()", addSemi = FALSE, retclass = "character")
  z <- x$LinAlgForm
  z <- setdiff(z, "i") # i = imaginary unit: I in Yacas, i in R
  return(z)
}