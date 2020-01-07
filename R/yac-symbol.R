#' Make a yacas symbol
#' 
#' Note that this results in multiple calls to `yacas` and 
#' the performance may be slower than manually using e.g. [yac_str()].
#' 
#' @param x A vector or a matrix
#' 
#' @return A `yac_symbol`
#' 
#' @concept yac_symbol
#' @export
ysym <- function(x) {
  # TODO: Consider NSE: e.g. ysym(4*x + 5*y) directly?
  
  stopifnot(is.vector(x) | is.matrix(x))
  
  cmd <- if (is.matrix(x) | length(x) > 1L) {
    as_y(x)
  } else {
    x
  }
  
  cmd <- as.character(cmd)
  # NOTE: Do this here or later?
  cmd <- yac_str(cmd)
  
  is_mat <- grepl("^[ ]*\\{[ ]*\\{.*\\}[ ]*\\}[ ]*$", cmd)
  is_vec <- !is_mat && grepl("^[ ]*\\{.*\\}[ ]*$", cmd)
  
  y <- list(yacas_cmd = cmd,
            is_mat = is_mat,
            is_vec = is_vec)
  
  class(y) <- c("yac_symbol", "list")
  return(y)
}

#' Make a yacas symbol
#' 
#' This is an alias for [ysym()]. 
#' See description there.
#' 
#' @param x A vector or a matrix
#' 
#' @return A `yac_symbol`
#' 
#' @concept yac_symbol
#' @export
yac_symbol <- function(x) {
  return(ysym(x)) 
}

#' List defined yac_symbols
#' 
#' @param print_details print content of symbols
#' 
#' @export
ysym_ls <- function(print_details = FALSE) {
  x <- ls(envir = .GlobalEnv)
  
  idx <- sapply(x, function(v) is(get(v, envir = .GlobalEnv), "yac_symbol"))
  
  if (!any(idx)) {
    return(invisible(c()))
  }
  
  res <- x[idx]
  
  if (print_details) {
    for (i in which(idx)) {
      v <- x[i]
      cat(v, ":\n", sep = "")
      print(get(v, envir = .GlobalEnv))
    }
    
    return(invisible(res))
  }
  
  return(res)
}

# Construct and assign a vector of variables
# 
# If unnamed vector, the content is the variables.
# 
# If named vector, the names are the variables, 
# and the values are the content.
# 
# Note, that it has the side effect that it assigns variables in 
# `.GlobalEnv`.
# 
# @param x Vector of variable names. If named, the names are the variables 
# and the content is the value.
# @param overwrite overwrite if exists already
# @param warn warn if name already exists in `.GlobalEnv`
# 
# @return Invisibly names assigned
# 
# @examples 
# ysym_ls()
# ysym_make(c("a", "b"))
# ysym_ls()
# a
# 
# ysym_make(c("xt" = "{{1, t}}", "xtk" = "{{1, t+k}}"))
# ysym_ls()
# xt
# ysym_ls(print_details = TRUE)
# 
# # Warning
# # ysym_make(c("a", "b"))
# ysym_make(c("a" = "2", "b" = "3"), overwrite = TRUE, warn = FALSE)
# a
# 
# @export
# ysym_make <- function(x, overwrite = FALSE, warn = TRUE) {
#   nms <- x
#   vals <- x
#   succ <- character(length(nms))
#   
#   if (!is.null(names(x))) {
#     nms <- names(x)
#   }
#   
#   res <- sapply(seq_along(nms), function(i) {
#     do_assign <- TRUE
#     
#     # Check if name already exists
#     if (exists(nms[i], envir = .GlobalEnv)) {
#       if (warn && overwrite) {
#         warning(paste0(nms[i], " already exists, overwriting"))
#         do_assign <- TRUE
#       } else if (warn && !overwrite) {
#         warning(paste0(nms[i], " already exists, skipping"))
#         do_assign <- FALSE
#       } else {
#         # warn == TRUE
#         do_assign <- overwrite
#       }
#     }
#     
#     if (do_assign) {
#       assign(nms[i], ysym(vals[i]), envir = .GlobalEnv)
#       succ[i] <- nms[i]
#     } else {
#       succ[i] <- NA_character_
#     }
#   })
#   
#   return(invisible(succ))
# }

#########################################
# S3 exports
#########################################

#' @export
yac_str.yac_symbol <- function(x) {
  return(yac_str(x$yacas_cmd))
}

#' @export
yac_expr.yac_symbol <- function(x) {
  return(yac_expr(x$yacas_cmd))
}

#' @export
yac_silent.yac_symbol <- function(x) {
  return(yac_silent(x$yacas_cmd))
}

#' @export
yac.yac_symbol <- function(x, rettype = c("str", "expr", "silent")) {
  return(yac(x$yacas_cmd, rettype))
}

#' @export
yac_assign.yac_symbol <- function(value, x) {
  return(yac_silent(paste0(x, " := ", value$yacas_cmd)))
}

#' @export
y_fn.yac_symbol <- function(x, fn, ...) {
  x <- y_fn(x$yacas_cmd, fn, ...)
  y <- yac_str(x)
  
  # Special case returning string
  if (fn == "TeXForm") {
    return(y)
  }
  
  z <- ysym(y)
  return(z)
}

#' @export
as_y.yac_symbol <- function(x) {
  return(x$yacas_cmd)
}

#' @export
as_r.yac_symbol <- function(x) {
  y <- yac_expr(x$yacas_cmd)

  if (expr_has_vars(y)) {
    return(y)
  }

  return(eval(y))
}

#' @export
dim.yac_symbol <- function(x) {
  if (!x$is_mat) {
    return(NULL)
  }
  
  #rows:=Length(matrix);
  rows <- as.integer(yac_str(y_fn(x$yacas_cmd, "Length")))
  
  # cols:=Length(matrix[1]);
  cols <- as.integer(yac_str(y_fn(paste0(x$yacas_cmd, "[1]"),"Length")))
  
  return(c(rows, cols))
}

#' @export
length.yac_symbol <- function(x) {
  if (!(x$is_vec || x$is_mat)) {
    #return(NULL)
    return(1L)
  }
  
  if (x$is_vec) {
    return(as.integer(yac_str(y_fn(x$yacas_cmd, "Length"))))
  }
  
  return(prod(dim(x)))
}



#' @export
c.yac_symbol <- function(...) {
  args <- list(...)

  for (i in seq_along(args)) {
    if (!is(args[[i]], "yac_symbol")) {
      args[[i]] <- ysym(args[[i]])
    }
  }
  
  elements <- lapply(args, function(x) {
    z <- x$yacas_cmd
    z <- gsub("[{}]*", "", z)
    return(z)
  })
  
  cmd <- paste0("{", paste0(elements, collapse = ", "), "}")
  
  v <- ysym(cmd)
  
  return(v)
}


#' Simplify expression
#' 
#' @param x A `yac_symbol`
#' @param timeout timeout in seconds before simplification is aborted; 
#' only works when package unix is available
#' 
#' @concept yac_symbol
#' 
#' @export
simplify <- function(x, timeout = 2) {
  UseMethod("simplify")
}

#' @export
simplify.yac_symbol <- function(x, timeout = 2) {
  stopifnot(is.null(timeout) || (!is.null(timeout) && timeout > 0))
  
  x_res <- ysym(yac_str(x$yacas_cmd))
  z <- y_fn(x_res$yacas_cmd, "Simplify")
  
  z_res <- NULL
  
  # unix available
  if (!is.null(timeout) && requireNamespace("unix", quietly = TRUE)) {
    z_res <- unix::eval_safe(yac_str(z), timeout = timeout)
  } else {
    z_res <- yac_str(z)
  }
  
  stopifnot(!is.null(z_res))
  
  v <- ysym(z_res)
  
  return(v)
}

#' Export object to TeX
#' 
#' @param x A `yac_symbol`
#' 
#' @concept yac_symbol
#' 
#' @export
tex <- function(x) {
  UseMethod("tex")
}

#' @export
tex.yac_symbol <- function(x) {
  x_res <- ysym(yac_str(x$yacas_cmd))
  
  z <- y_fn(x_res$yacas_cmd, "TeXForm")
  z_res <- yac_str(z)
  
  # Trim
  z_res <- gsub("^[ ]*", "", z_res)
  z_res <- gsub("[ ]*$", "", z_res)
  
  return(z_res)
}

#' Matrix multiplication
#' 
#' @param x A `yac_symbol`
#' @param y A `yac_symbol`
#' 
#' @concept yac_symbol
#' 
#' @export
`%*%` <- function(x, y) {
  UseMethod("%*%")
}

#' @export
`%*%.default` <- function(x, y) {
  return(base::`%*%`(x, y))
}

#' @export
`%*%.yac_symbol` <- function(x, y) {
  stopifnot(methods::is(x, "yac_symbol"))
  stopifnot(methods::is(y, "yac_symbol"))
  
  x_res <- ysym(yac_str(x$yacas_cmd))
  y_res <- ysym(yac_str(y$yacas_cmd))
  
  z <- paste0(x_res$yacas_cmd, " * ", y_res$yacas_cmd)
  z_res <- yac_str(z)
  
  v <- ysym(z_res)
  
  return(v)
}

#' Matrix diagonals
#' 
#' From [base::diag()].
#' 
#' @param x If `yac_symbol` treat as such, else call [base::diag()].
#' @param \dots further arguments passed to [base::diag()]
#' 
#' @concept yac_symbol
#' 
#' @export
diag <- function(x, ...) {
  UseMethod("diag")
}

#' @export
diag.default <- function(x, ...) {
  return(base::diag(x = x, ...))
}

#' @export
diag.yac_symbol <- function(x, ...) {
  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)
  
  stopifnot(y$is_mat)
  
  w <- as_r(y$yacas_cmd)
  z <- base::diag(w, ...)
  v <- ysym(as_y(z))
  
  return(v)
}




#' Matrix diagonals
#' 
#' From [base::diag()].
#' 
#' @param x If `yac_symbol` treat as such, else call `base::diag<-()`.
#' @param value New value for `diag(x)`
#' 
#' @concept yac_symbol
#' 
#' @export
`diag<-` <- function(x, value) {
  UseMethod("diag<-")
}

#' @export
`diag<-.default` <- function(x, value) {
  return(base::`diag<-`(x = x, value = value))
}

#' @export
`diag<-.yac_symbol` <- function(x, value) {
  stopifnot(methods::is(x, "yac_symbol"))

  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)
  stopifnot(y$is_mat)

  w <- as_r(y$yacas_cmd)
  
  z <- base::`diag<-`(w, value)
  
  v <- ysym(as_y(z))
  
  return(v)
}


#' Lower and upper triangular part of a matrix
#' 
#' @param x If `yac_symbol` treat as such, else 
#' call [base::lower.tri()]/[base::upper.tri()].
#' @param diag Whether diagonal is included.
#' 
#' @concept yac_symbol
#' 
#' @export
upper.tri <- function(x, diag = FALSE) {
  UseMethod("upper.tri")
}

#' @export
upper.tri.default <- function(x, diag = FALSE) {
  return(base::upper.tri(x = x, diag = diag))
}

#' @export
upper.tri.yac_symbol <- function(x, diag = FALSE) {
  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)
  
  stopifnot(y$is_mat)
  
  w <- as_r(y$yacas_cmd)
  return(base::upper.tri(x = w, diag = diag))
}

#' Lower and upper triangular part of a matrix
#' 
#' @param x If `yac_symbol` treat as such, else 
#' call [base::lower.tri()]/[base::upper.tri()].
#' @param diag Whether diagonal is included.
#' 
#' @concept yac_symbol
#' 
#' @export
lower.tri <- function(x, diag = FALSE) {
  UseMethod("lower.tri")
}

#' @export
lower.tri.default <- function(x, diag = FALSE) {
  return(base::lower.tri(x = x, diag = diag))
}

#' @export
lower.tri.yac_symbol <- function(x, diag = FALSE) {
  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)
  
  stopifnot(y$is_mat)
  
  w <- as_r(y$yacas_cmd)
  return(base::lower.tri(x = w, diag = diag))
}


#' @title t
#' @name t
#' 
#' @param x If `yac_symbol` treat as such, else 
#' call [base::t()].
#' 
#' @concept yac_symbol
#' @export
t.yac_symbol <- function(x) {
  # To see if x is indeed a matrix
  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)
  
  stopifnot(y$is_mat)
  
  z <- y_fn(x = y_res, fn = "Transpose")
  v <- ysym(yac_str(z))
  
  return(v)
}

solve_yac_symbol_matrixinverse <- function(a) {
  stopifnot(a$is_mat)
  
  z <- y_fn(x = a, fn = "Inverse")
  z_res <- yac_str(z)
  v <- ysym(z_res)
  
  return(v)
}

solve_yac_symbol_linearsolve <- function(a, b) {
  stopifnot(a$is_mat)
  stopifnot(b$is_vec)
  
  stopifnot(nrow(a) == length(b))
  
  cmd <- paste0("SolveMatrix(", a$yacas_cmd, ", ", b$yacas_cmd, ")")
  v <- ysym(cmd)
  
  return(v)
}
  
#' Solve a system of equations
#' 
#' This generic function solves the 
#' equation $a x = b$ for $x$.
#' 
#' When `a` is a matrix and `b` not provided,
#' this finds the inverse of `a`.
#' When `a` is a matrix and a vector `b` is provided, the 
#' linear system of equations is solved.
#' 
#' Note that solving non-linear equations:
#' 
#' * `solve(a, b)`: find roots of `a` for variable `b`, i.e. yacas `Solve(a == 0, b)`
#' * `solve(a, b, v)`: find solutions to `a == b` for variable `v`, i.e. yacas `Solve(a == b, v)`
#' 
#' This also works for a system of equations (when `a` is a vector)
#' 
#' @param a A `yac_symbol` 
#' @param b A `yac_symbol` or a value, see details and examples.
#' @param \dots See details and examples.
#' 
#' @examples 
#' A <- outer(0:3, 1:4, "-") + diag(2:5)
#' a <- 1:4
#' B <- ysym(A)
#' b <- ysym(a)
#' solve(A)
#' solve(B)
#' solve(A, a)
#' solve(B, b)
#' 
#' poly <- ysym("x^2 - x - 6")
#' solve(poly, "x")    # Solve(poly == 0, x)
#' solve(poly, 3, "x") # Solve(poly == 3, x)
#' 
#' @export
solve.yac_symbol <- function(a, b, ...) {
  if (!is(a, "yac_symbol")) {
    stop("'a' must be a yac_symbol")
  }

  # If only a given:
  if (missing(b)) {
    # If a matrix:
    if (a$is_mat) {
      return(solve_yac_symbol_matrixinverse(a))
    } else if (a$is_vec) {
      stop("a cannot be a yac_symbol vector")
    } else if (!a$is_vec) {
      # a not matrix nor vector
      stop("Trying to find roots? Please provide variable name, too")
    } else {
      stop("Should not occur")
    }
  } else {
    # b also given:
    
    if (a$is_mat) {
      if (!is(b, "yac_symbol")) {
        stop("'b' must be a yac_symbol.")
      }
      if (!b$is_vec) {
        stop("'b' must be a vector")
      }
      
      return(solve_yac_symbol_linearsolve(a, b))
    }
    
    # System of equations
    if (!a$is_mat && a$is_vec) {
      dots <- list(...)
      
      n <- length(a)
      
      b_yac <- if (is(b, "yac_symbol")) {
        b$yacas_cmd
      } else {
        paste0("{", paste0(b, collapse = ", "), "}")
      }

      rhs <- if (length(dots) == 0L) {
        paste0("{", paste0(rep("0", n), collapse = ", "), "}")
      } else {
        if (length(b) != n) {
          stop("Unequal number of LHSs and RHSs")
        }
        b_yac
      } 
      
      vars <- if (length(dots) == 0L) {
        b_yac
      } else {
        v <- unlist(dots)
        paste0("{", paste0(v, collapse = ", "), "}")
      }
      
      eqs_lhs <- y_hlp_from_yacvec(a$yacas_cmd)
      eqs_rhs <- y_hlp_from_yacvec(rhs)
      stopifnot(length(eqs_lhs) == length(eqs_rhs))
      eqs <- paste0("{", paste0(eqs_lhs, "==", eqs_rhs, collapse = ", "), "}")

      cmd <- paste0("Solve(", eqs, ", ", vars, ")")
      res <- ysym(cmd)
      return(res)
    }
    
    if (!a$is_mat && !a$is_vec) {
      dots <- list(...)
      
      if (length(dots) == 0L) {
        if (is.character(b) && length(b) == 1L) {
          # Solve(a, b)
          cmd <- paste0("Solve(", a$yacas_cmd, ", ", b, ")")
          res <- ysym(cmd)
          return(res)
        }
      } else if (length(dots) == 1L) {
        v <- dots[[1L]]
        
        if (is.character(v) && length(v) == 1L) {
          # Solve(a == b, v)
          cmd <- paste0("Solve(", a$yacas_cmd, " == ", b, ", ", v, ")")
          res <- ysym(cmd)
          return(res)
        }
      } 
    }
  }
  
  stop("Could not recognise the way that solve() was tried used on yac_symbol's.")
}


#' Extract or replace parts of an object
#' 
#' @param x A `yac_symbol`.
#' @param i row indices specifying elements to extract or replace
#' @param j column indices specifying elements to extract or replace
#' 
#' @export
`[.yac_symbol` <- function(x, i, j) {
  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)
  
  stopifnot(y$is_mat | y$is_vec)
  
  if (y$is_vec && !missing(j)) {
    stop("Cannot specify second dimension for a vector")
  }
  
  w <- as_r(y$yacas_cmd)
  z <- NULL
  
  if (y$is_vec) {
    z <- base::`[`(x = w, i = i)
  } else if (y$is_mat) {
    if (missing(j)) {
      n_args <- nargs()
      
      if (n_args == 2L) {
        # x[1:2]
        z <- base::`[`(x = w, i = i)
      } else if (n_args == 3L) {
        # x[1:2,]
        z <- base::`[`(x = w, i = i, )
      }
    } else {
      # x[1:2,2]
      z <- base::`[`(x = w, i = i, j = j)
    }
  }
  
  stopifnot(!is.null(z))
  v <- ysym(z)
  
  return(v)
}

#' Extract or replace parts of an object
#' 
#' @param x A `yac_symbol`.
#' @param i row indices specifying elements to extract or replace
#' @param j column indices specifying elements to extract or replace
#' @param value the value to replace `x[i, j]` by
#' 
#' @export
`[<-.yac_symbol` <- function(x, i, j, value) {
  # NOTE: 
  # quote? To enable e.g.
  # x[i, j] <- a
  # for some symbol a

  stopifnot(methods::is(x, "yac_symbol"))
  
  y_res <- yac_str(x$yacas_cmd)
  y <- ysym(y_res)
  
  stopifnot(y$is_mat | y$is_vec)
  
  if (y$is_vec && !missing(j)) {
    stop("Cannot specify second dimension for a vector")
  }
  
  w <- as_r(y$yacas_cmd)
  z <- NULL

  if (y$is_vec) {
    z <- `[<-`(x = w, i = i, value = value)
  } else if (y$is_mat) {
    if (missing(j)) {
      n_args <- nargs()

      if (n_args == 3L) {
        # x[1:2] <- ...
        z <- base::`[<-`(x = w, i = i, value = value)
      } else if (n_args == 4L) {
        # x[1:2,] <- ...
        allj <- seq_len(ncol(w))
        z <- base::`[<-`(x = w, i = i, j = allj, value = value)
      }
      
    } else {
      z <- `[<-`(x = w, i = i, j = j, value = value)
    }
  }
  
  stopifnot(!is.null(z))
  v <- ysym(as_y(z))
  
  return(v)
}




#' Extract parts of an object
#' 
#' @param x A `yac_symbol`.
#' @param i indices specifying elements to extract
#' 
#' @export
`[[.yac_symbol` <- function(x, i) {
  stopifnot(methods::is(x, "yac_symbol"))
  stopifnot(x$is_vec)
  
  stopifnot(!is.null(i))
  stopifnot(length(i) == 1L)
  i <- as.integer(i)
  
  stopifnot(i >= 1L)
  stopifnot(i <= length(x))
  
  v <- y_fn(x, "Nth", i)
  
  return(v)
}


#' @export
print.yac_symbol <- function(x, ...) {
  y_res <- yac_str(x$yacas_cmd)
  #cat("Yacas symbol: ", yac_str(x$yacas_cmd), "\n", sep = "")
  Ryacas::y_print(y_res)
  return(invisible(x))
}

#' @importFrom utils str
#' @export
str.yac_symbol <- function(object, ...) {
  x <- object
  class(x) <- "list"
  return(str(x))
}

#' @export
y_rmvars.yac_symbol <- function(x) {
  # FIXME: Best API?
  y <- y_rmvars(x$yacas_cmd)
  v <- ysym(y)
  
  return(v)
}

#' Math operators
#' 
#' @param e1 A `yac_symbol`.
#' @param e2 A `yac_symbol`.
#' 
#' @export
Ops.yac_symbol = function(e1, e2) {
  if (!(.Generic %in% c("+", "-", "*", "/", "^"))) {
    stop("Function '", .Generic, "' not yet implemented for yac_symbol")
  } 
  
  if (missing(e2)) {
    if (.Method[1] == "") {
      e1 <- ysym(e1)
    }
    
    txt <- paste0(.Generic, "(", e1$yacas_cmd, ")")
    txt_res <- yac_str(txt)
    x <- ysym(txt_res)
    return(x)
  }
  
  # Both e1 and e2 given:
  
  # LHS constant, e.g. 2*x: e1 is a number 2
  if (.Method[1] == "") {
    e1 <- ysym(e1)
  }
  
  # RHS constant, e.g. x*2: e2 is a number 2
  if (.Method[2] == "") {
    e2 <- ysym(e2)
  }
  
  txt <- paste0("(", e1$yacas_cmd, ")", .Generic, "(", e2$yacas_cmd, ")")
  txt_res <- yac_str(txt)
  x <- ysym(txt_res)
  
  return(x)
}

Math_transtab <- matrix( c(
  #R					yacas
  "sin",			"Sin",
  "cos",			"Cos",
  "tan",			"Tan",
  
  "asin",	  	"ArcSin",
  "acos",	  	"ArcCos",
  "atan",    	"ArcTan",
  "asinh", 	  "ArcSinh", 
  "acosh", 	  "ArcCosh", 
  "atanh",   	"ArcTanh",
  
  "exp", 	  	"Exp",
  "log", 	  	"Ln",
  "sqrt", 	  "Sqrt"
), byrow = TRUE, ncol = 2)
colnames(Math_transtab) <- c("R", "yacas")
#paste0(Math_transtab[, 1], "()", collapse = ", ")

#' Math functions
#' 
#' @param x `yac_symbol`.
#' @param \dots further arguments passed to methods
#' 
#' @export
Math.yac_symbol = function(x, ...) {
  i <- match(.Generic, Math_transtab[, 1L])
  
  if (is.na(i)) {
    stop("Function '", .Generic, "' not yet implemented for yac_symbol")
  } 
  
  fn <- Math_transtab[i, 2L]
  
  txt <- paste0(fn, "(", x$yacas_cmd, ")")
  x <- ysym(txt)
  return(x)
}


#' Find the derivative of yac symbol
#' 
#' @param expr A `yac_symbol`
#' @param \dots variables as character vector to take derivate with respect to
#' 
#' @concept yac_symbol
#' 
#' @export
deriv.yac_symbol <- function(expr, ...) {
  vars <- unlist(list(...))
  
  res <- unlist(lapply(vars, function(var) {
    paste0("(D(", var, ") ", expr$yacas_cmd, ")")
  }))
  
  res_sym <- ysym(res)
  
  return(res_sym)
}

#' Find the Jacobian matrix of yac symbol
#' 
#' @param expr A `yac_symbol`
#' @param \dots variables as character vector to take Jacobian with respect to
#' 
#' @concept yac_symbol
#' 
#' @export
Jacobian <- function(expr, ...) {
  UseMethod("Jacobian")
}

#' @export
Jacobian.yac_symbol <- function(expr, ...) {
  vars <- unlist(list(...))
  
  res <- paste0("JacobianMatrix( ", expr$yacas_cmd, ", {", 
                paste0(vars, collapse = ", "), "})")
  
  res_sym <- ysym(res)
  return(res_sym)
}

#' Find the Hessian matrix of yac symbol
#' 
#' @param expr A `yac_symbol`
#' @param \dots variables as character vector to take Hessian with respect to
#' 
#' @concept yac_symbol
#' 
#' @export
Hessian <- function(expr, ...) {
  UseMethod("Hessian")
}

#' @export
Hessian.yac_symbol <- function(expr, ...) {
  vars <- unlist(list(...))
  
  res <- paste0("HessianMatrix(", expr$yacas_cmd, ", {", 
                paste0(vars, collapse = ", "), "})")
  
  res_sym <- ysym(res)
  return(res_sym)
}

#' Convert yac symbol to character
#' 
#' @param x A `yac_symbol`
#' @param \dots not used
#' 
#' @concept yac_symbol
#' 
#' @export
as.character.yac_symbol <- function(x, ...) {
  return(x$yacas_cmd)
}







bound_to_str <- function(b) {
  if (is.infinite(b)) {
    if (b > 0) {
      return("Infinity")
    } else {
      return("-Infinity")
    }
  }
  
  if (is.character(b)) {
    return(b)
  }
  
  bnd <- deparse(eval(substitute(substitute(b)), parent.frame()))
  bnd <- gsub("pi", "Pi", bnd, fixed = TRUE)
  
  return(bnd)
}




#' Integration of Functions
#' 
#' If `f` is a `yac_symbol`, `yacas`'s `Integrate()` is used. 
#' Else, [stats::integrate()] is used.
#' 
#' Additional arguments:
#' 
#' * [`yac_symbol`]: `var`, `lower`, `upper`
#' * Else ([stats::integrate()]): `lower`, `upper`
#' 
#' @param f Function to integrate. See details.
#' @param \dots See details.
#' 
#' @concept yac_symbol
#' 
#' @export
integrate <- function(f, ...) {
  UseMethod("integrate")
}

#' @importFrom stats integrate
#' @export
integrate.default <- function(...) {
  x <- stats::integrate(...)
  return(x)
}

#' @export
integrate.yac_symbol <- function(f, var, lower, upper, ...) {
  cmd <- if (missing(lower) && missing(upper)) {
    paste0("Integrate(", var, ")")
  } else {
    lwr_str <- bound_to_str(lower)
    upr_str <- bound_to_str(upper)
    paste0("Integrate(", var, ", ", lwr_str, ", ", upr_str, ")")
  }
  
  z <- paste0(cmd, f$yacas_cmd)
  z_res <- yac_str(z)
  
  v <- ysym(z_res)
  
  return(v)
}

flat_op <- function(expr, op, identity) {
  z <- expr
  
  while (grepl("{", z$yacas_cmd, fixed = TRUE)) {
    z <- y_fn(z, "UnFlatten", op, identity)
  }
  
  return(z)
}


#' Product of Vector Elements
#' 
#' @param expr Expression to be multiplied
#' @param \dots Not used
#' @param na.rm Not used
#' 
#' @concept yac_symbol
#' 
#' @export
prod.yac_symbol <- function(expr, ..., na.rm = FALSE) {
  z <- flat_op(expr, '"*"', '1')
  return(z)
}


#' Summation
#' 
#' If only `expr` given: sum elements.
#' 
#' Else: sums `expr` by letting `var` taking values 
#' from `lower` to `upper` (potentially `Inf`)
#' 
#' @param expr Expression to be summed
#' @param var Variable to sum
#' @param lower Lower limit
#' @param upper Upper limit
#' @param \dots Not used
#' @param na.rm Not used
#' 
#' @concept yac_symbol
#' 
#' @export
sum.yac_symbol <- function(expr, var, lower, upper, ..., na.rm = FALSE) {
  if (missing(var) && missing(lower) && missing(upper)) {
    z <- flat_op(expr, '"+"', '0')
    return(z)
  }
  
  lwr_str <- bound_to_str(lower)
  upr_str <- bound_to_str(upper)
  
  cmd <- paste0("Sum(", var, ", ", lwr_str, ", ", upr_str, ", ", expr$yacas_cmd, ")")
  z_res <- yac_str(cmd)
  
  v <- ysym(z_res)
  
  return(v)
}



#' Combine R Objects by Rows
#' 
#' @param \dots Objects to bind
#' @param deparse.level Not used
#' 
#' @concept yac_symbol
#' 
#' @export
rbind.yac_symbol <- function(..., deparse.level = 1) {
  args <- list(...)
  ls <- lapply(args, length)
  
  if (length(unique(ls)) != 1L) {
    stop("All must have same length")
  }
  
  v <- unlist(lapply(args, function(x) {
    if (x$is_mat) {
      stop("Cannot bind matrices")
    }
    
    # x$is_mat == FALSE
    if (!x$is_vec) {
      return(paste0("{", x$yacas_cmd, "}"))
    }
    
    return(x$yacas_cmd)
  }))
  
  z <- paste0("{", paste0(v, collapse = ", "), "}")
  z <- ysym(z)
  
  return(z)
}


#' Combine R Objects by Columns
#' 
#' @param \dots Objects to bind
#' @param deparse.level Not used
#' 
#' @concept yac_symbol
#' 
#' @export
cbind.yac_symbol <- function(..., deparse.level = 1) {
  # Not efficient, but easy for now...
  return(t(rbind(..., deparse.level = deparse.level)))
}




#' Limits
#' 
#' If first argument is a `yac_symbol`, `yacas`'s `Limit()` is used. 
#' 
#' Arguments:
#' 
#' * [`yac_symbol`]: `f`, `var`, `val`, `from_left`, `from_right`
#' 
#' @param \dots See details.
#' 
#' @concept yac_symbol
#' 
#' @export
lim <- function(...) {
  UseMethod("lim")
}

#' @export
lim.default <- function(...) {
  stop("Not implemented for anything else than yac_symbol's created with ysym()")
}

#' @export
lim.yac_symbol <- function(f, var, val, ...) {
  dots <- list(...)
  
  val_str <- bound_to_str(val)

  cmd_add <- if (!is.null(dots$from_left) && length(dots$from_left) == 1L && dots$from_left == TRUE) {
    ", Left"
  } else if (!is.null(dots$from_right) && length(dots$from_right) == 1L && dots$from_right == TRUE) {
    ", Right"
  } else {
    ""
  }
  
  cmd <- paste0("Limit(", var, ", ", val_str, cmd_add, ") (", f$yacas_cmd, ")")
  
  z_res <- yac_str(cmd)
  
  v <- ysym(z_res)
  
  return(v)
}




#' Give a variable a value
#' 
#' @param x yac_symbol
#' @param var Variable
#' @param val Value
#' 
#' @concept yac_symbol
#' 
#' @export
with_value <- function(x, var, val) {
  UseMethod("with_value")
}

#' @export
with_value.default <- function(x, var, val) {
  stop("Not implemented for anything else than yac_symbol's created with ysym()")
}

#' @export
with_value.yac_symbol <- function(x, var, val) {
  # WithValue(var, val, expr)
  
  cmd <- paste0("WithValue(", var, ", ", 
                as.character(val), ", ", 
                as.character(x), ")")
  
  z_res <- yac_str(cmd)
  
  v <- ysym(z_res)
  
  return(v)
}

