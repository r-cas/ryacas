#' Pretty print yacas strings
#' 
#' @param x yacas string, e.g. a matrix
#' 
#' @examples 
#' A <- diag(4)
#' Ayac <- as_y(A)
#' y_print(Ayac)
#' 
#' B <- A
#' B[2, 2] <- "-t"
#' Byac <- as_y(B)
#' Byac
#' y_print(Byac)
#' 
#' @concept helper
#' 
#' @export
y_print <- function(x) {
  if (grepl("^\\{\\{.*\\}\\}$", x)) {
    # Matrix
    z <- y_hlp_to_yacmat_print(as_r(x))
    cat(z, "\n")
  } else if (grepl("^\\{.*\\}$", x)) {
    # Vector
    z <- y_hlp_to_yacvec(as_r(x))
    cat(z, "\n")
  } else {
    # Something else:
    #print(x)
    #print(paste0("y: ", x), quote = FALSE)
    cat("y: ", x, "\n", sep = "")
  }

  return(invisible(x))
}

