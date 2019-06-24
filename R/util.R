strip_start_end_quotes <- function(x) {
  x <- gsub("^\\\"", "", x)
  x <- gsub(";[[:blank:]]*$", "", x)
  x <- gsub("\\\"$", "", x)
  
  return(x)
}

#' Get width of yacas output
#' 
#' @concept yac_communication
#' @keywords internal
#' 
#' @export
get_output_width <- function() {
  res <- yacas("Print(FormulaMaxWidth())")
  res <- gsub("\n", "", res$PrettyForm, fixed = TRUE)
  res <- as.integer(res)
  return(res)
}

#' Set width of yacas output
#' 
#' @param w Width in number of characters
#' 
#' @concept yac_communication
#' @keywords internal
#' 
#' @export
set_output_width <- function(w) {
  cmd <- paste0("SetFormulaMaxWidth(", w, ")")
  return(invisible(yacas(cmd)))
}

