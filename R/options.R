RYACAS_OPTIONS <- settings::options_manager(
  module_matvec_enabled = TRUE,
  prettyform_default = FALSE, 
  .allowed = list(
    module_matvec_enabled = settings::inlist(TRUE, FALSE),
    prettyform_default = settings::inlist(TRUE, FALSE)
  )
)

#' Set or get options for the Ryacas package
#' 
#' @param ... Option names to retrieve option values or `[key] = [value]` pairs to set options.
#'
#' @section Supported options:
#' The following options are supported
#' 
#' * `module_matvec_enabled` (default `TRUE`): Print yacas `List()`'s as 
#'   vectors and `List(List(), ...)`'s as matrices.
#' * `prettyform_default` (default `FALSE`): Print yacas as `PrettyForm()` as default.
#'
#' @importFrom settings stop_if_reserved
#' @export
Ryacas_options <- function(...) {
  # protect against the use of reserved words.
  settings::stop_if_reserved(...)
  RYACAS_OPTIONS(...)
}
