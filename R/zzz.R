.onLoad <- function(libname, pkgname){
  if (requireNamespace("pkgload", quietly = TRUE)) {
    if (pkgload::is_dev_package("Ryacas")) {
      # Package was loaded using pkgload/devtools
      #path <- pkgload:::shim_system.file(package = "Ryacas", "yacas")
      # Uses pkgload:::shim_system.file because pkgload overwrites system.file
      path <- system.file(package = "Ryacas", "yacas")
      
      # Force initialise so that a new instance is created.
      # Especially useful during development with e.g. devtools::load_all().
      yacas_init_force_path(path, ryacas_init = TRUE)
    }
  }
}

