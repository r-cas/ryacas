
yacasFile <- function(filename = c("yacas.exe", "scripts.dat", "R.ys"), 
   slash = c("\\", "/")) {
   stopifnot(.Platform$OS.type == "windows")
   whole.path <- function(path, defpath, deffile) {
      if (path == "") path <- defpath
      if (file.info(path)$isdir) 
         file.path(sub("[/\\]$", "", path), deffile) 
      else path
   }
   yacas.exe <- whole.path(Sys.getenv("YACAS_HOME"),
         system.file(package = "Ryacas", "yacdir"), "yacas.exe")
   fullname <- switch(match.arg(filename), yacas.exe = yacas.exe,
      scripts.dat = whole.path(Sys.getenv("YACAS_SCRIPTS"),
         dirname(yacas.exe), "scripts.dat"), 
      R.ys = whole.path(Sys.getenv("YACAS_INIT"),
	 system.file(package = "Ryacas", "yacdir"), "R.ys"))
   slash <- match.arg(slash)
   chartr(setdiff(c("/", "\\"), slash), slash, fullname)
}

yacasCheck <- function(yacas.size = 368640, scripts.size = 224035) {
   # return: NA = dont know, -1 = not found, 0 = ok, 1 = found but wrong version
   stopifnot(.Platform$OS.type == "windows")
   yacas.invoke.string <- Sys.getenv("YACAS_INVOKE_STRING")
   if (yacas.invoke.string != "") return(NA)
   yacas.exe <- yacasFile("yacas.exe")
   scripts.dat <- yacasFile("scripts.dat")
   if (file.exists(yacas.exe) && file.exists(scripts.dat)) {
      if (file.info(scripts.dat) == scripts.size &&
         file.info(yacas.exe)$size == yacas.size) 0
      else 1
   } else -1
}
   
yacasInstall <- function(showonly = FALSE, ...) {
   stopifnot(.Platform$OS.type == "windows")
   urlbase <- "http://ryacas.googlecode.com/svn/trunk/inst/yacdir"
   # yacdir <- system.file(package = "Ryacas", "yacdir")
   files <- c("scripts.dat", "yacas.exe")
   lapply(files, function(f) {
      cat("Download", file.path(urlbase, f), "\nto", yacasFile(f), "\n")
      if (!showonly)
         download.file(file.path(urlbase, f), yacasFile(f), mode = "wb", ...) 
      }
   )
   result <- yacasCheck()
   if (is.na(result)) 
      warning("The checks for yacas existence and version were bypassed.\n")
   if (result == -1) warning("yacas was not found.\n")
   if (result == 1) warning("Different version of yacas found than expected.\n")
   invisible()
}

