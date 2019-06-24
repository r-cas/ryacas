# Matrices / vectors

y_hlp_to_yacmat <- function(x) {
  z <- paste0("{", apply(x, 1, paste0, collapse = ", "), "}")
  z <- paste0("{", paste0(z, collapse = ", "), "}")
  z
}

y_hlp_to_yacmat_print <- function(x) {
  if (nrow(x) == 1L) {
    return(y_hlp_to_yacmat(x))
  }
  
  x_width <- max(nchar(x))
  
  x_padded <- format(x, width = x_width, justify = "right")
  
  z <- paste0("{", apply(x_padded, 1, paste0, collapse = ", "), "}")
  
  z_first <- paste0("{", z[1L])
  z_last <- paste0(" ", z[length(z)], "}")
  z_middle <- NULL

  if (length(z) > 2L) {
    z_middle <- paste0(" ", z[-c(1L, length(z))])
  }

  z <- c(z_first, z_middle, z_last)
  z <- paste0(z, collapse = ",\n")
  return(z)
}

y_hlp_to_yacvec <- function(x) {
  z <- paste0("{", paste0(x, collapse = ", "), "}")
  z
}

y_hlp_from_yacmat <- function(x) {
  z <- strsplit(x, "\\}[ ]*,[ ]*\\{")
  z <- z[[1L]]
  z[1L] <- gsub("^\\{[ ]*\\{", "", z[1L])
  z[length(z)] <- gsub("\\}[ ]*\\}$", "", z[length(z)])
  w <- strsplit(z, ",")
  w <- lapply(w, function(ww) gsub("[[:blank:]]", "", ww))
  u <- do.call(rbind, w)
  return(u)
}

y_hlp_from_yacvec <- function(x) {
  z <- strsplit(x, ",")
  z <- z[[1L]]
  z[1L] <- gsub("^\\{", "", z[1L])
  z[length(z)] <- gsub("\\}$", "", z[length(z)])
  z <- gsub("[[:blank:]]", "", z)
  return(z)
}

