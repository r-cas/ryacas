
.First.lib <- function(lib, pkg, ...) {
   # library.dynam("Ryacas", pkg, lib)
   yacdir <- system.file(package = "Ryacas", "yacdir")
   yacas.exe <- file.path(yacdir, "yacas.exe")
   if (.Platform$OS.type == "windows" && !file.exists(yacas.exe)) {
      cat("Issue this command to install yacas: yacasInstall()\n")
   }
   invisible()
}
