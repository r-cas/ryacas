.onLoad <- function(libname, pkgname){
  if (requireNamespace("pkgload", quietly = TRUE)) {
    if (pkgload::is_dev_package("ryacas")) {
      # Package was loaded using pkgload/devtools
      
      # Force initialise so that a new instance is created.
      # Especially useful during development with e.g. devtools::load_all().
      .yacas_init_force()
    }
  }
}