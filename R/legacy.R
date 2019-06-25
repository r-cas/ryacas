# if (FALSE) {
#   exports <- '
# S3method("%Where%",default)
# S3method(Clear,Expr)
#   S3method(Clear,default)
#   S3method(Conjugate,Expr)
#   S3method(Conjugate,default)
#   S3method(Eval,Expr)
#   S3method(Eval,Sym)
#   S3method(Eval,default)
#   S3method(Eval,yacas)
#   S3method(Expand,Expr)
#   S3method(Expand,default)
#   S3method(Factor,Expr)
#   S3method(Factor,default)
#   S3method(Factorial,Expr)
#   S3method(Factorial,default)
#   S3method(Identity,default)
#   S3method(Integrate,Expr)
#   S3method(Integrate,default)
#   S3method(Inverse,Expr)
#   S3method(Inverse,default)
#   S3method(InverseTaylor,default)
#   S3method(Limit,Expr)
#   S3method(Limit,default)
#   S3method(List,Expr)
#   S3method(List,default)
#   S3method(Math,Expr)
#   S3method(Math,Sym)
#   S3method(N,Expr)
#   S3method(N,default)
#   S3method(Newton,Expr)
#   S3method(Newton,default)
#   S3method(Ops,Expr)
#   S3method(Ops,Sym)
#   S3method(Precision,Expr)
#   S3method(Precision,default)
#   S3method(PrettyForm,Expr)
#   S3method(PrettyForm,default)
#   S3method(PrettyPrinter,Expr)
#   S3method(PrettyPrinter,default)
#   S3method(Simplify,Expr)
#   S3method(Simplify,default)
#   S3method(Solve,Expr)
#   S3method(Solve,default)
#   S3method(Subst,default)
#   S3method(Taylor,Expr)
#   S3method(Taylor,default)
#   S3method(TeXForm,default)
#   S3method(Transpose,default)
#   S3method(Ver,Expr)
#   S3method(Ver,default)
#   S3method(as.Sym,Expr)
#   S3method(as.Sym,character)
#   S3method(as.Sym,matrix)
#   S3method(as.Sym,yacas)
#   S3method(as.character,Expr)
#   S3method(as.character,Sym)
#   S3method(as.character,yacas)
#   S3method(as.expression,Sym)
#   S3method(as.expression,yacas)
#   S3method(deriv,Expr)
#   S3method(deriv,Sym)
#   S3method(determinant,Expr)
#   S3method(determinant,Sym)
#   S3method(print,Expr)
#   S3method(print,Sym)
#   S3method(print,yacas)
#   S3method(yacas,"function")
#   S3method(yacas,character)
#   S3method(yacas,expression)
#   S3method(yacas,formula)
#   S3method(yacas,yacas)
#   export("%Where%")
#   export(Clear)
#   export(Conjugate)
#   export(Eval)
#   export(Expand)
#   export(Expr)
#   export(Exprq)
#   export(Factor)
#   export(Factorial)
#   export(I)
#   export(Identity)
#   export(Infinity)
#   export(Integrate)
#   export(Inverse)
#   export(InverseTaylor)
#   export(Limit)
#   export(List)
#   export(N)
#   export(Newton)
#   export(Pi)
#   export(Precision)
#   export(PrettyForm)
#   export(PrettyPrinter)
#   export(Ryacas_options)
#   export(Set)
#   export(Simplify)
#   export(Solve)
#   export(Subst)
#   export(Sym)
#   export(Taylor)
#   export(TeXForm)
#   export(Transpose)
#   export(Ver)
#   export(as.Sym)
#   export(as.language)
#   export(getSyms)
#   export(get_output_width)
#   export(root)
#   export(set_output_width)
#   export(stripvar)
#   export(syacas)
#   export(y_ls)
#   export(yacas)
#   export(yacas_evaluate)
#   export(yacmode)
#   '
#   x <- strsplit(exports, "\n")[[1L]]
#   y <- gsub("^.*\\(([^,]*)[,]*.*\\).*$", "\\1", x)
#   y <- gsub(" ", "", y, fixed = TRUE)
#   cbind(x, y)
#   z <- c("get_output_width", "set_output_width", "as.character", "print")
#   w <- setdiff(unique(y[nchar(y) > 0L]), z)
#   w <- c(w, "as.expression.Sym", "as.expression.yacas")
#   #w
# 
# cat(
# paste0(paste0("#' @inherit dep_msg\n#' @concept legacy\n#'\n#' @export\n", w, ' <- function(...) {\n  stop(stop(dep_msg()))\n}\n'), collapse = "\n"),
# sep = ''
# )
# 
# }

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
Math <- function(...) {
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
Ops <- function(...) {
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
