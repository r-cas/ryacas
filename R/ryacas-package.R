#' R interface to yacas computer algebra package
#' 
#' Ryacas allows one to use the yacas computer algebra package entirely from
#' within R. 
#' 
#' Please read the "Getting started" vignette.
#' 
#' @name Ryacas-package
#' @useDynLib Ryacas, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats deriv
#' @importFrom methods is
#' @keywords programming
"_PACKAGE"


#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
magrittr::`%>%`

