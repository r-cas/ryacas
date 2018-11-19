RYACAS_OPTIONS <- settings::options_manager(
  module_matvec_enabled = TRUE, 
  .allowed = list(
    module_matvec_enabled = settings::inlist(TRUE, FALSE)
  )
)

#' Set or get options for the Ryacas package
#' 
#' @param ... Option names to retrieve option values or `[key] = [value]` pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' 
#' * `module_matvec_enabled` (default `TRUE`): Print yacas `List()`'s as vectors and 
#'
#' @export
Ryacas_options <- function(...) {
  # protect against the use of reserved words.
  stop_if_reserved(...)
  RYACAS_OPTIONS(...)
}