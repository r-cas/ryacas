#' Legacy function
#' 
#' @param \dots not used
#' 
#' @concept legacy
#' @keywords internal
dep_msg <- function() {
  str <- paste0(c("This function is defunct in this version of Ryacas, but still exists in the legacy package Ryacas0 (https://github.com/mikldk/ryacas0) if you need it.",
                "Please refer to the vignette 'Getting started' (http://mikldk.github.io/ryacas/articles/getting-started.html) for information on how this new, improved version of Ryacas works.",
                "This function will be removed entirely in next version of Ryacas."), 
                collapse = "\n\n")
  return(str)
}

#' @export
"%Where%" <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#' 
#' @aliases %Where%
#'
#' @export
Clear <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Conjugate <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Eval <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Expand <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Factor <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Factorial <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Identity <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Integrate <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Inverse <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
InverseTaylor <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Limit <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
List <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
N <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Newton <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Precision <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
PrettyForm <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
PrettyPrinter <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#' @rdname SimplifyOld
#'
#' @export
Simplify <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Solve <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Subst <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Taylor <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
TeXForm <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Transpose <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Ver <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
as.Sym <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
deriv <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
determinant <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
yacas <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Expr <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Exprq <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
I <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Infinity <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Pi <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Ryacas_options <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Set <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
Sym <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
as.language <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
getSyms <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
root <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
stripvar <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
syacas <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
y_ls <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
yacas_evaluate <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
yacmode <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#' 
#' @export
as.expression.Sym <- function(...) {
  stop(dep_msg())
}

#' @inherit dep_msg
#' @concept legacy
#'
#' @export
as.expression.yacas <- function(...) {
  stop(dep_msg())
}
